import game.Ctx2D
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import sun.security.action.GetLongAction

import scala.scalajs.js
import collection.mutable.Buffer
import scala.util.Random

object Color {
  val red    = "#DF0024"
  val yellow = "#F3C300"
  val green  = "#00AC9F"
  val blue   = "#2E6DB4"
}

case class Bullet(x: Double,
                  y: Double,
                  dx: Double,
                  dy: Double,
                  color: String,
                  damage: Double = 10.0,
                  isFoe: Boolean = true,
                  var isHit: Boolean = false) {
  def draw(implicit ctx: Ctx2D): Unit = {
    ctx.beginPath
    ctx.arc(x, y, 5, 0, Math.PI * 2)
    ctx.fillStyle = color
    ctx.fill
  }

  def setHit = isHit = true
}

abstract class BattleShip(xx: Double,
                          yy: Double,
                          ctx: Ctx2D,
                          rotate: Boolean = true,
                          val isFoe: Boolean = true,
                          shootRainBow: Boolean = false) {
  var health                            = 100.0;
  var energy                            = 100.0;
  protected implicit val context: Ctx2D = ctx;
  protected var dx                      = 0.0;
  protected var dy                      = 0.0;
  var x                                 = xx;
  var y                                 = yy;
  protected var alpha                   = Math.PI / 2 * 3;
  protected var dAlpha = Random.nextDouble() * Math.PI * 0.1 * (if (Random.nextGaussian() > 0) -1
                                                                else 1)
  val shipColor                  = Utils.getRandomColor
  protected lazy val bulletColor = Utils.getRandomColor

  protected val fixSize = 30.0;
  protected var size    = fixSize;
  protected val dSize   = Random.nextDouble() * 3;

  updatePostition(x, y)

  def drawShip()

  def turn = {
    if (rotate) {
      alpha = (alpha + dAlpha) % (2 * Math.PI)
      size = fixSize + Math.sin(alpha) * dSize
    }
  }

  def damage(b: Bullet): Unit = {
    val distance = Math.pow(b.x - this.x, 2) + Math.pow(b.y - this.y, 2)
    if (distance <= Math.pow(this.size, 2) && b.isFoe != this.isFoe) {
      this.health -= b.damage
      b.setHit
    }
  }

  def damageShip(s: BattleShip): Unit = {
    val distance = Math.pow(s.x - this.x, 2) + Math.pow(s.y - this.y, 2)
    if (distance <= Math.pow(this.size, 2) && s.isFoe != this.isFoe) this.health = 0
  }

//  private def updateBulletStates = {
//    allBullets = allBullets.map(b => b.copy(x = b.x + b.dx, y = b.y + b.dy))
//    allBullets = allBullets.filter(b =>
//      b.y < browserStuff.height + 10 && b.y > 0 && b.x < browserStuff.width + 10 && b.x > 0)
//  }

  def fire() = {
    val delta                    = Random.nextDouble() * 10
    val horizontalBulletVelocity = (Random.nextGaussian() * 10) % 1
    var bullet = if (this.isFoe) {
      Bullet(this.x + delta,
             this.y,
             this.dx + horizontalBulletVelocity,
             +30 + this.dy,
             bulletColor,
             isFoe = this.isFoe)
    } else {
      Bullet(this.x + delta,
             this.y,
             this.dx + horizontalBulletVelocity,
             -30 + this.dy,
             bulletColor,
             isFoe = this.isFoe)
    }

    bullet = if (this.rotate) {
      bullet.copy(dx = this.dx + this.size * Math.cos(this.alpha),
                  dy = this.dy + this.size * Math.sin(this.alpha))
    } else bullet

    bullet = if (this.shootRainBow) {
      bullet.copy(color = Utils.getRandomColor)
    } else bullet

    Global.allBullets += bullet
  }

  def updatePostition(x: Double, y: Double) = {
    this.dx = x - this.x
    this.dy = y - this.y
    this.x = x
    this.y = y
  }

  def foesMoves(): Unit = {
    this.x += (Random.nextGaussian() * 10) % 10
    this.y += Random.nextDouble() * 10
  }
}

class Circle(xx: Double,
             yy: Double,
             ctx: Ctx2D,
             rotate: Boolean = true,
             isFoe: Boolean = true,
             shootRainBow: Boolean = false)
    extends BattleShip(xx, yy, ctx, rotate, isFoe, shootRainBow) {
  def drawShip(): Unit = {
    ctx.beginPath
    ctx.arc(x, y, size, 0, Math.PI * 2)
    ctx.fillStyle = this.shipColor
    ctx.fill
  }
}

class Triangle(xx: Double,
               yy: Double,
               ctx: Ctx2D,
               rotate: Boolean = true,
               isFoe: Boolean = true,
               shootRainBow: Boolean = false)
    extends BattleShip(xx, yy, ctx, rotate, isFoe, shootRainBow) {
  val fixAngel = 2.0 / 3.0 * Math.PI

  def drawShip(): Unit = {
    var (ax, ay) = (x + size * Math.cos(alpha), this.y + size * Math.sin(alpha))
    var (bx, by) =
      (x + size * Math.cos(alpha + fixAngel), this.y + size * Math.sin(alpha + fixAngel))
    var (cx, cy) =
      (x + size * Math.cos(alpha + 2 * fixAngel), this.y + size * Math.sin(alpha + 2 * fixAngel))

    ctx.beginPath
    ctx.moveTo(ax, ay)
    ctx.lineTo(bx, by)
    ctx.lineTo(cx, cy)
    ctx.fillStyle = this.shipColor
    ctx.fill
  }
}

object Global {
  var allBullets = Buffer[Bullet]()

  def addBullets(b: Bullet) = allBullets += b
  def updateBulletPosition = {
    allBullets = allBullets.map(b => b.copy(x = b.x + b.dx, y = b.y + b.dy))
    allBullets.foreach(_.draw(browserStuff.ctx))
  }
}

object browserStuff {
  val width  = dom.window.innerWidth
  val height = dom.window.innerHeight
  val canvas = document.createElement("canvas")
  canvas.setAttribute("width", dom.window.innerWidth.toString)
  canvas.setAttribute("height", dom.window.innerHeight.toString)
  canvas.setAttribute("id", "ctx")
  document.body.appendChild(canvas)

  implicit val c: html.Canvas = document.getElementById("ctx").asInstanceOf[html.Canvas]
  implicit val ctx = c
    .getContext("2d")
    .asInstanceOf[Ctx2D]
}

object Foes {
  import browserStuff._
  var x = 0
  def addFoe(foes: Buffer[BattleShip]): Unit = {
    (Random.nextInt() % 4) match {
      case 0 => foes += new Circle(browserStuff.width * Random.nextDouble(), 0, ctx)
      case _ => foes += new Triangle(browserStuff.width * Random.nextDouble(), 0, ctx)
    }
    x += 50
    foes --= foes.filter(_.y > height + 50)
  }
}

object Utils {

  def getRandomColor: String =
    (Math.abs(Random.nextInt) % 4) match {
      case 0 => Color.red
      case 1 => Color.yellow
      case 2 => Color.green
      case _ => Color.blue
    }

  def checkDamage(ship: BattleShip, foes: Buffer[BattleShip]): Boolean = {
    // invoke bullet damage
    Global.allBullets.foreach(b => {
      ship.damage(b)
      foes.foreach(_.damage(b))
    })

//    println(s"checking damage: ")
//    Global.allBullets.foreach(println)
//    println("====")

    // invoke ship damage
    foes.foreach(_.damageShip(ship))

    // remove broken ship
    foes --= foes.filter(_.health <= 0)
    // remove used bullet
    Global.allBullets --= Global.allBullets.filter(_.isHit)

    if (ship.health <= 0) true // game over
    else false
  }

}

object game {
  type Ctx2D =
    dom.CanvasRenderingContext2D
  def clearCtx(implicit ctx: Ctx2D, canvas: html.Canvas) =
    ctx.clearRect(0, 0, canvas.width, canvas.height)

  def main(args: Array[String]): Unit = {
    import browserStuff._

    val ship =
      new Triangle(dom.window.innerWidth / 2, dom.window.innerHeight * 0.9, ctx, false, false, true)
    val foes = Buffer[BattleShip]()

    dom.document.onmousemove = (e: dom.MouseEvent) => {
      ship.updatePostition(e.clientX, e.clientY)
    }

    dom.document.addEventListener("touchmove", (e: dom.TouchEvent) => {
      ship.updatePostition(e.touches(0).clientX, e.touches(0).clientY)
    })

    dom.window.setInterval(
      () => {
        if (foes.length < 2) Foes.addFoe(foes)
        foes --= foes.filter(f => f.y > browserStuff.height + 20)
      },
      4000
    )

    dom.window.setInterval(
      () => {
        foes.foreach(_.foesMoves())
      },
      50
    )

    dom.window.setInterval(
      () => {
        foes.foreach(_.turn)
      },
      70
    )

    dom.window.setInterval(
      () => {
        Global.updateBulletPosition
      },
      50
    )

    dom.window.setInterval(
      () => {
        ship.fire()
        Utils.checkDamage(ship, foes)
      },
      100
    )

    dom.window.setInterval(
      () => {
        foes.foreach(_.fire())
      },
      150
    )

    dom.window.setInterval(() => {
      clearCtx
      Global.allBullets.foreach(_.draw)
      ship.drawShip()
      foes.foreach(_.drawShip())
    }, 10)

  }

}
