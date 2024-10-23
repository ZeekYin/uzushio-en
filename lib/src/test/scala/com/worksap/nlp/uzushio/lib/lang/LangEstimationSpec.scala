package com.worksap.nlp.uzushio.lib.lang

import java.nio.charset.{Charset, StandardCharsets}
import org.scalatest.freespec.AnyFreeSpec
import scala.io.Source

class LangEstimationSpec extends AnyFreeSpec {

  "LangEstimation" - {
    val estimator = new LangEstimation()

    "detects Japanese language from a simulated Wikipedia page about Japan" in {
      // 模拟维基百科介绍日本的 HTML 页面，并用日语书写，Shift-JIS 编码
      val htmlContent = """
        <html>
          <head>
            <title>日本 - Wikipedia</title>
          </head>
          <body>
            <h1>日本</h1>
            <p>日本（にっぽん、にほん）は、東アジアに位置する島国で、太平洋に面しています。日本は北海道、本州、四国、九州の四つの主要な島から構成されています。</p>
            <p>日本の首都は東京で、人口は世界でも有数の規模を誇ります。日本は高度に発展した国であり、技術、経済、文化など多くの分野で世界に影響を与えています。</p>
            <p>日本の歴史は古く、何世紀にもわたる様々な変革と発展を遂げてきました。現代の日本は、明治維新後に急速に産業化され、世界的な経済大国となりました。</p>
            <p>第二次世界大戦後、日本は驚異的な復興を遂げ、現在では世界で最も強力な経済の一つとして知られています。</p>
          </body>
        </html>
      """
      val data = htmlContent.getBytes("Shift_JIS")
      val result = estimator.estimateLang(data, 0, Charset.forName("Shift_JIS"))
      
      // 断言检测结果应该是日语
      assert(result.isInstanceOf[ProbableLanguage])
      assert(result.asInstanceOf[ProbableLanguage].lang == "ja") // 期待的结果是日语
    }

    "detects English language from a real HTML file" in {
      // 从文件中读取 HTML 内容
      val source = Source.fromResource("en_page.html")(StandardCharsets.UTF_8)
      val htmlContent = try source.mkString finally source.close()
      
      val data = htmlContent.getBytes(StandardCharsets.UTF_8)
      val result = estimator.estimateLang(data, 0, StandardCharsets.UTF_8)
      
      // 断言检测结果应该是英语
      assert(result.isInstanceOf[ProbableLanguage])
      assert(result.asInstanceOf[ProbableLanguage].lang == "en") // 期待的结果是英语
    }
  }
}