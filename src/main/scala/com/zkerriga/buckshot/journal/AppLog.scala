package com.zkerriga.buckshot.journal

import izumi.logstage.api.Log
import izumi.logstage.api.rendering.logunits.Renderer
import izumi.logstage.api.rendering.{RenderingPolicy, logunits}
import izumi.logstage.sink.file.FileServiceImpl.RealFile
import izumi.logstage.sink.file.models.{FileRotation, FileSinkConfig}
import izumi.logstage.sink.file.{FileService, FileSink}
import logstage.IzLogger

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object AppLog:
  private val instance: IzLogger = IzLogger(
    threshold = Log.Level.Trace,
    sink = new FileSink[RealFile](
      renderingPolicy = RenderingPolicy.colorlessPolicy(
        Some(
          Renderer.Aggregate(
            units = Seq(
              logunits.Extractor.Level(1),
              logunits.Extractor.Space,
              logunits.Extractor.LoggerContext(),
              logunits.Extractor.Space,
              logunits.Extractor.Message(),
            ),
          ),
        ),
      ),
      fileService = new FileService[RealFile] {
        val RunId: String = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd_HH-mm-ss"))
        override val path: String = "logs"

        override def createFileWithName: String => RealFile = _ => RealFile(s"$path/log-$RunId.log")
      },
      rotation = FileRotation.DisabledRotation,
      config = FileSinkConfig.inBytes(1024 * 1024),
    ) {
      override def recoverOnFail(e: String): Unit = println(e)
    },
  )

  /** probably a silly way to instantiate loggers, but I just need something simple
    *
    * later, I may create some solid journal mechanism to track game progress
    */
  def create(name: String): IzLogger =
    instance("name" -> name)

  trait Logging:
    final val log: IzLogger = create(this.getClass.getSimpleName.stripSuffix("$").split('.').last)
