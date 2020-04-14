import game.Ctx2D
import javax.swing.plaf.basic.BasicGraphicsUtils
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import collection.mutable.Buffer


case class Bullet(x: Double, y: Double, xV: Double, yV: Double) {
  def draw(implicit ctx: Ctx2D): Unit = {
    ctx.beginPath
    ctx.arc(x, y, 5, 0, Math.PI * 2)
    ctx.fillStyle = "#FF0000"
    ctx.fill
  }
}


class BattleShip(var x: Double, var y: Double, ctx: Ctx2D) {
  private var health = 100.0;
  private var energy = 100.0;
  private var allBullets = collection.mutable.Buffer[Bullet]()
  private implicit val context: Ctx2D = ctx;
  private var xV = 0.0;
  private var xY = 0.0;

  updatePostition(x, y)

  def drawBullet() = {
    allBullets.foreach(_.draw)
  }

  def drawShip()= {
    ctx.beginPath
    ctx.arc(x, y, 20, 0, Math.PI * 2)
    ctx.fillStyle = "#000000"
    ctx.fill
  }

  private def updateBulletStates = {
    allBullets = allBullets.map(b => b.copy(x = b.x + b.xV, y = b.y + b.yV)).filter(b => b.y > 0)
  }

  def fire() = {

    val bullet = Bullet(this.x, this.y, this.xV, -30 + this.xY)
    allBullets += bullet
    updateBulletStates
  }

  def updatePostition(x: Double, y: Double)= {
    this.xV = x - this.x
    this.xY = y - this.y
    this.x = x
    this.y = y
  }

}


object game {
  type Ctx2D =
    dom.CanvasRenderingContext2D
  def clearCtx(implicit ctx: Ctx2D, canvas: html.Canvas) = ctx.clearRect(0, 0, canvas.width, canvas.height)

  def main(args: Array[String]): Unit = {
//    val canvas = document.createElement("canvas")
//    canvas.setAttribute("width", dom.window.innerWidth.toString)
//    canvas.setAttribute("height", dom.window.innerHeight.toString)
//    canvas.setAttribute("id", "ctx")
//    document.body.appendChild(canvas)
//    implicit val c: html.Canvas = document.getElementById("ctx").asInstanceOf[html.Canvas]
//    implicit val ctx = c.getContext("2d")
//      .asInstanceOf[Ctx2D]

//    val ship = new BattleShip(dom.window.innerWidth / 2,  dom.window.innerHeight * 0.9, ctx)

//    dom.document.onmousemove = (e: dom.MouseEvent) => {
//      ship.updatePostition(e.clientX, e.clientY)
//    }


    dom.document.addEventListener("touchmove", (e: dom.raw.Touch) => {
      val p = document.createElement("p")
      p.textContent = "hello: " + s"${e} ${e.clientX} ${e.clientY}"
      document.body.appendChild(p)
    })

//    dom.window.setInterval(
//      () => ship.fire(),
//      50
//    )
//
//    dom.window.setInterval(
//      () => {
//        clearCtx
//        ship.drawBullet()
//        ship.drawShip()
//      },
//      5)
//
  }

}
