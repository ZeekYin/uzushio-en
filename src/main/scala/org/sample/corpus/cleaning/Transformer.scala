package org.sample.corpus.cleaning

import org.apache.spark.sql.Dataset

/** Transforms given spark dataset. */
trait Transformer extends scala.Serializable {
  def transform(ds: Dataset[Seq[String]]): Dataset[Seq[String]]
}

/** Transformer that does nothing. */
class IdentityTransformer extends Transformer {
  override def transform(ds: Dataset[Seq[String]]): Dataset[Seq[String]] = ds
}

/** Sequencially apply multiple transformers.
  *
  * @param stages
  *   list of transformers to apply
  */
class Pipeline(private var stages: Seq[Transformer] = Seq())
    extends Transformer {
  def setStages(value: Seq[Transformer]): Pipeline = {
    stages = value
    this
  }

  override def transform(ds: Dataset[Seq[String]]): Dataset[Seq[String]] = {
    stages.foldLeft(ds)((ds, tr) => tr.transform(ds))
  }
}
