package com.worksap.nlp.uzushio.lib.lang

import com.optimaize.langdetect.LanguageDetectorBuilder
import com.optimaize.langdetect.ngram.NgramExtractor
import java.nio.charset.{Charset, CodingErrorAction}
import java.nio.{ByteBuffer, CharBuffer}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

sealed trait EstimationResult {
  def str: String = "unk"
}
case object BadEncoding extends EstimationResult
case object EstimationFailure extends EstimationResult
case class ProbableLanguage(lang: String) extends EstimationResult {
  override def str: String = lang
}

class LangEstimation(private val minBytes: Int = 256) {
  private val internalBuffer = CharBuffer.allocate(5 * 1024)
  private val decodeBuffer = CharBuffer.allocate(4 * 1024)
  private def langDetector = LangEstimation.cachedDetector

  /** Extract visible text from HTML content using Jsoup
    * @param html
    *   input HTML as a string
    * @return
    *   cleaned string with only visible text
    */
  private def extractVisibleText(html: String): String = {
    // parse html
    val doc: Document = Jsoup.parse(html)

    // remove script and style tags
    doc.select("script, style").remove()

    // extract visible text
    val visibleText = doc.body().text()
    println(s"Extracted visible text (first 100 chars): ${visibleText.take(100)}...")
    visibleText
  }

  /** Copy meaningful content into detection buffer, using Jsoup to extract visible text.
    *
    * @param input
    *   input CharBuffer
    * @param output
    *   output CharBuffer
    */
  private def copyMeaningfulContent(input: CharBuffer, output: CharBuffer): Unit = {
    // Extract visible text from the input buffer
    val content = input.toString
    val visibleText = extractVisibleText(content)

    // Clean the visible text
    val meaningfulContent = visibleText.flatMap { char =>
      if (char.isLetterOrDigit || char.isWhitespace || char >= 128) {
        Some(char)
      } else {
        None
      }
    }
    // Put the cleaned content into the output buffer
    val result = meaningfulContent.mkString.trim
    println(s"Meaningful content: $result") // Print the meaningful content
    // Copy meaningful content to the output buffer
    output.put(result)
  }

  private def prepareBuffer(
      bytes: Array[Byte],
      offset: Int,
      charset: Charset
  ): Option[Int] = {
    val decBuf = decodeBuffer
    val buf = internalBuffer
    val inputData = ByteBuffer.wrap(bytes, offset, (bytes.length - offset).min(20 * 1024))
    val decoder = charset.newDecoder().onUnmappableCharacter(CodingErrorAction.REPORT)
    decBuf.clear()
    buf.clear()

    while (inputData.remaining() > 0 && buf.remaining() > 0) {
      val result = decoder.decode(inputData, decBuf, true)
      if (result.isUnmappable || result.isError || result.isMalformed) {
        return None
      }
      decBuf.flip()
      copyMeaningfulContent(decBuf, buf) // Copy meaningful content to the detection buffer
      decBuf.clear()
    }

    buf.flip()
    println(s"Copied characters: ${buf.limit()}") //debug
    Some(buf.limit())
  }

  /** Estimate language by taking at most 5k characters from first 20kb of text.
    * Retains both ASCII and non-ASCII characters, but removes HTML and JavaScript tags.
    * Returns [[BadEncoding]] if there exist non-mappable characters using the passed encoding.
    *
    * @param data
    *   text to detect language from
    * @param offset
    *   offset from the array start
    * @param charset
    *   charset to use for converting byte stream to characters
    * @return
    *   child classes of [[EstimationResult]]
    */
  def estimateLang(
      data: Array[Byte],
      offset: Int,
      charset: Charset
  ): EstimationResult = {
    val bufferStatus = prepareBuffer(data, offset, charset)
    if (bufferStatus.isEmpty) {
      return BadEncoding
    }
    val ncopied = bufferStatus.get
    println(s"Copied characters: $ncopied") // debug
    if (ncopied > minBytes) {
      val language = langDetector.detect(internalBuffer)
      println(s"Detected language: ${language}") // debug
      if (!language.isPresent) {
        EstimationFailure
      } else {
        val code = language.get().getLanguage
        println(s"Detected language code: $code") // debug
        ProbableLanguage(code)
      }
    } else {
      EstimationFailure
    }
  }
}

object LangEstimation {

  private lazy val cachedDetector = {
    val builtinLangs = com.optimaize.langdetect.profiles.BuiltInLanguages.getLanguages
    val profileReader = new com.optimaize.langdetect.profiles.LanguageProfileReader
    val profiles = profileReader.readBuiltIn(builtinLangs)
    LanguageDetectorBuilder.create(NgramExtractor.gramLengths(1, 2)).withProfiles(profiles).build()
  }

}