import scalashop.RGBA

val a = Array(1, 2, 5, 10)

a(2) = 30


class Img(val width: Int, val height: Int, private val data: Array[Int]) {
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): Int = data(y * width + x)
  def update(x: Int, y: Int, c: Int): Unit = data(y * width + x) = c
}

val i = new Img(5, 3)

i.toString(" ")


i.update(1, 1, 10)
