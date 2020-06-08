package scalashop

import org.scalameter._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    val points = for {
      x <- 0 until src.width
      y <- from until end
    } yield(x,y)

    points.map(point => dst.update(point._1,point._2, boxBlurKernel(src,point._1,point._2,radius)))

  }

  def listFromToEnd(last: Int, numTasks: Int) = {
    val num = Math.min(last, numTasks)
    val mod = last % num
    val l = if (mod == 0) {
      0 to last by last/num
    } else {
      (0 to last-mod by (last-mod)/(num-1)) ++ List(last)
    }
    l zip l.tail
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val strips = listFromToEnd(src.height, numTasks)
    val tasks = strips map { x => task (blur(src, dst, x._1, x._2, radius))}
    tasks map (t => t.join())
  }

}
