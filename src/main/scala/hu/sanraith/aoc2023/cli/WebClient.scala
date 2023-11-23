package hu.sanraith.aoc2023.cli

import java.net._
import java.net.http.HttpResponse._
import java.net.http._
import java.nio.file._
import java.time.Duration
import scala.util._

class WebClient(sessionCookie: String) {
  private val CacheDir = ".cache"
  private val AdventOfCodeRoot = "https://adventofcode.com/"
  private val UserAgent =
    "https://github.com/sanraith/aoc2023 by sanraith@users.noreply.github.com"

  private val client =
    val cookie = new HttpCookie("session", sessionCookie);
    cookie.setPath("/")
    cookie.setVersion(0)

    CookieHandler.setDefault(new CookieManager());
    CookieHandler
      .getDefault()
      .asInstanceOf[CookieManager]
      .getCookieStore()
      .add(new URI(AdventOfCodeRoot), cookie);

    HttpClient
      .newBuilder()
      .connectTimeout(Duration.ofSeconds(20))
      .cookieHandler(CookieHandler.getDefault())
      .build()

  def requestCached(subUrl: String, invalidateCache: Boolean = false): Option[String] =
    val cachedFileName = s"${subUrl.replace("/", "_")}.txt"
    val cachedFilePath = Paths.get(CacheDir, cachedFileName)
    val url = URI(AdventOfCodeRoot).resolve(subUrl).toURL()

    Try((FileManager.readUtf8File(cachedFilePath), invalidateCache)) match
      case (Success(body, false)) =>
        println(s"Using cached '${cachedFilePath.toString}' instead of $url")
        Some(body)

      case _ =>
        request(url) match
          case (200, body) =>
            FileManager.writeToUtf8File(cachedFilePath, body)
            Some(body)
          case (statusCode, _) =>
            println(s"Status code $statusCode returned from $url")
            None

  private def request(url: URL): (Int, String) =
    println(s"Loading $url")
    val request = HttpRequest
      .newBuilder(url.toURI)
      .setHeader("User-Agent", UserAgent)
      .build();
    val response = client.send(request, BodyHandlers.ofString)
    (response.statusCode, response.body)
}
