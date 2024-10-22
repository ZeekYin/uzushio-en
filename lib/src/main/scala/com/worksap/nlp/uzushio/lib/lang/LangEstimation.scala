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
    // 使用 Jsoup 解析 HTML 文档
    val doc: Document = Jsoup.parse(html)

    // 移除 script 和 style 元素
    doc.select("script, style").remove()

    // 提取页面可见文本
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
    // 将输入转换为字符串并提取可见文本
    val content = input.toString
    val visibleText = extractVisibleText(content)

    // 过滤并清理剩下的文本内容，保留字母、数字、空格以及非 ASCII 字符
    val meaningfulContent = visibleText.flatMap { char =>
      if (char.isLetterOrDigit || char.isWhitespace || char >= 128) {
        Some(char)
      } else {
        None
      }
    }

    // 打印有意义的内容 (前 100 个字符)
    println(s"Meaningful content (first 100 chars): ${meaningfulContent.take(100)}...")

    // 确保有意义的内容不为空并写入到输出缓冲区
    if (meaningfulContent.nonEmpty) {
      output.put(meaningfulContent.mkString)
    }

    output.flip() // 确保缓冲区准备好读取
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
      copyMeaningfulContent(decBuf, buf) // 将清理后的内容写入 `internalBuffer`
      decBuf.clear()
    }

    buf.flip()
    println(s"Copied characters: ${buf.limit()}") // 打印已复制的字符数量
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
    println(s"Copied characters: $ncopied") // 打印已复制的字符数量
    if (ncopied > minBytes) {
      val language = langDetector.detect(internalBuffer)
      println(s"Detected language: ${language}") // 打印探测到的语言
      if (!language.isPresent) {
        EstimationFailure
      } else {
        val code = language.get().getLanguage
        println(s"Detected language code: $code") // 打印探测到的语言代码
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