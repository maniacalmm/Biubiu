import Global.ship
import browserStuff.{ctx, height, width}
import game.{Ctx2D, start}
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.MouseEvent
import sun.security.action.GetLongAction

import scala.scalajs.js
import collection.mutable.Buffer
import scala.util.Random

//TODO: the bullet hits foe, set foe back a bit, so the shooting feels better

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
                  var damage: Double = 10.0,
                  isFoe: Boolean = true,
                  var isHit: Boolean = false,
                  val size: Int = 10) {
  def draw(implicit ctx: Ctx2D): Unit = {
    ctx.beginPath
    ctx.arc(x, y, size, 0, Math.PI * 2)
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
  protected implicit val context: Ctx2D = ctx;
  protected var dx                      = 0.0;
  protected var dy                      = 0.0;
  var x                                 = xx;
  var y                                 = yy;
  protected var alpha                   = Math.PI / 2 * 3;
  protected var dAlpha = Random.nextDouble() * Math.PI * 0.3 * (if (Random.nextGaussian() > 0) -1
                                                                else 1)
  protected var bulletSpeed      = 25 + Random.nextGaussian() % 10
  val shipColor                  = Utils.getRandomColor
  protected lazy val bulletColor = Utils.getRandomColor

  protected val fixSize      = Math.max(browserStuff.height * 0.025, 50);
  protected var size         = fixSize;
  protected val dSize        = Random.nextDouble() * 3;
  protected var bulletDamage = 10.0;
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
      if (this.isFoe) {
        Global.score += 10
        Global.energy += 10
        if (Global.energy > 100) {
          Global.energy = 100
        }
      }
      if (this.health > 0) this.health -= b.damage
      b.setHit
    }
  }

  def damageShip(s: BattleShip): Unit = {
    val distance = Math.pow(s.x - this.x, 2) + Math.pow(s.y - this.y, 2)
    if (distance <= Math.pow(this.size, 2) && s.isFoe != this.isFoe) this.health = 0
  }

  def fire() = {
    val delta                    = Random.nextDouble() * 10
    val horizontalBulletVelocity = (Random.nextGaussian() * 10) % 1
    var bullet = if (this.isFoe) {
      Bullet(this.x + delta,
             this.y,
             this.dx + horizontalBulletVelocity,
             this.dy + this.bulletSpeed,
             "#000000",
             damage = this.bulletDamage,
             isFoe = this.isFoe)
    } else {
      Bullet(this.x + delta,
             this.y,
             this.dx + horizontalBulletVelocity,
             -this.bulletSpeed + this.dy,
             bulletColor,
             damage = this.bulletDamage,
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

  def setBulletDamage(d: Double) = this.bulletDamage = d
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

class Cross(xx: Double,
            yy: Double,
            ctx: Ctx2D,
            rotate: Boolean = true,
            isFoe: Boolean = true,
            shootRainBow: Boolean = false)
    extends BattleShip(xx, yy, ctx, rotate, isFoe, shootRainBow) {
  val fixAngel = Math.PI / 2

  def drawShip(): Unit = {

    var (ax, ay) = (x + size * Math.cos(alpha), this.y + size * Math.sin(alpha))
    var (bx, by) =
      (x + size * Math.cos(alpha + fixAngel), this.y + size * Math.sin(alpha + fixAngel))
    var (cx, cy) =
      (x + size * Math.cos(alpha + 2 * fixAngel), this.y + size * Math.sin(alpha + 2 * fixAngel))
    var (dx, dy) =
      (x + size * Math.cos(alpha + 3 * fixAngel), this.y + size * Math.sin(alpha + 3 * fixAngel))

    ctx.beginPath
    ctx.lineWidth = this.size / 2
    ctx.moveTo(ax, ay)
    ctx.lineTo(cx, cy)
    ctx.strokeStyle = this.shipColor
    ctx.stroke()

    ctx.moveTo(bx, by)
    ctx.lineTo(dx, dy)
    ctx.strokeStyle = this.shipColor
    ctx.stroke()
  }
}

class Rectangle(xx: Double,
                yy: Double,
                ctx: Ctx2D,
                rotate: Boolean = true,
                isFoe: Boolean = true,
                shootRainBow: Boolean = false)
    extends BattleShip(xx, yy, ctx, rotate, isFoe, shootRainBow) {
  val fixAngel = Math.PI / 2

  def drawShip(): Unit = {
    var (ax, ay) = (x + size * Math.cos(alpha), this.y + size * Math.sin(alpha))
    var (bx, by) =
      (x + size * Math.cos(alpha + fixAngel), this.y + size * Math.sin(alpha + fixAngel))
    var (cx, cy) =
      (x + size * Math.cos(alpha + 2 * fixAngel), this.y + size * Math.sin(alpha + 2 * fixAngel))
    var (dx, dy) =
      (x + size * Math.cos(alpha + 3 * fixAngel), this.y + size * Math.sin(alpha + 3 * fixAngel))

    ctx.beginPath
    ctx.moveTo(ax, ay)
    ctx.lineTo(bx, by)
    ctx.lineTo(cx, cy)
    ctx.lineTo(dx, dy)
    ctx.fillStyle = this.shipColor
    ctx.fill
  }
}

object Global {
  var allBullets = Buffer[Bullet]()
  var score      = 0;
  var energy     = 0.0;

  var ship =
    new Triangle(dom.window.innerWidth / 2, dom.window.innerHeight * 0.9, ctx, false, false, true)
  ship.setBulletDamage(30)
  var foes = Buffer[BattleShip]()

  def addBullets(b: Bullet) = allBullets += b
  def updateBulletPosition = {
    allBullets = allBullets.map(b => b.copy(x = b.x + b.dx, y = b.y + b.dy))
    allBullets.foreach(_.draw(browserStuff.ctx))
  }

  def drawScore(ctx: Ctx2D) = {
    ctx.beginPath()
    ctx.font = "50px Verdana"
    ctx.fillText(this.score.toString, browserStuff.width - 200, browserStuff.height * 0.08)
  }

  def drawHealthBar(ship: BattleShip, ctx: Ctx2D) = {
    ctx.beginPath()
    ctx.lineWidth = 2
    ctx.rect(40, 40, 200, 20)
    ctx.strokeStyle = ship.shipColor
    ctx.stroke()
    ctx.fillStyle = ship.shipColor
    ctx.fillRect(40, 40, ship.health / 100 * 200, 20)
  }

  def drawEnergy(ship: BattleShip, ctx: Ctx2D) = {
    ctx.beginPath()
    ctx.lineWidth = 2
    ctx.rect(40, 70, 200, 20)
    ctx.strokeStyle = ship.shipColor
    ctx.stroke()
    ctx.fillStyle = ship.shipColor
    ctx.fillRect(40, 70, Global.energy / 100 * 200, 20)
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
      case 1 => foes += new Triangle(browserStuff.width * Random.nextDouble(), 0, ctx)
      case 2 => foes += new Cross(browserStuff.width * Random.nextDouble(), 0, ctx)
      case _ => foes += new Rectangle(browserStuff.width * Random.nextDouble(), 0, ctx)
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

  def bulletHit(b1: Bullet, b2: Bullet): Boolean = {
    (Math.pow(b1.x - b2.x, 2) + Math.pow(b1.y - b2.y, 2) + 3) <= Math.pow(b1.size + b2.size, 2)
  }

  def checkDamage(ship: BattleShip, foes: Buffer[BattleShip]): Boolean = {

    // invoke bullet damage
    Global.allBullets.foreach(b => {
      ship.damage(b)
      foes.foreach(_.damage(b))
    })
    // invoke ship damage
    foes.foreach(f => ship.damageShip(f))
    // ship bullets can fight off foes' bullets
    val shipBullet = Global.allBullets.filter(!_.isFoe)
    val foeBullet  = Global.allBullets.filter(_.isFoe)
    shipBullet.foreach(s => foeBullet.foreach(f => if (bulletHit(s, f)) { s.setHit; f.setHit }))

    // remove broken ship
    foes --= foes.filter(_.health <= 0)
    // remove used bullet
    Global.allBullets --= Global.allBullets.filter(_.isHit)

    if (ship.health <= 0) true // game over
    else false
  }

}

object PopUp {
  object EndGame {
    val margin = 5
    val msg    = "..yes?"
    val y      = browserStuff.height * 0.5
    val x      = (width - ctx.measureText(msg).width) / 2.0
    val x1     = x - margin
    val y1     = y - margin
    val x2     = x1 + ctx.measureText(msg).width + margin
    val y2     = y1 + 60 + margin
  }
}

object game {
  type Ctx2D =
    dom.CanvasRenderingContext2D

  val intervalId = Buffer.empty[Int]
  def clearCtx(implicit ctx: Ctx2D, canvas: html.Canvas) =
    ctx.clearRect(0, 0, canvas.width, canvas.height)

  val mouseMoveEvent =
    (e: dom.MouseEvent) => {
      if (Global.ship != null) Global.ship.updatePostition(e.clientX, e.clientY - 15)
    }

  val touchMoveEvent =
    (e: dom.TouchEvent) => {
      if (ship != null) ship.updatePostition(e.touches(0).clientX, e.touches(0).clientY - 100)
    }

  def startAgainEvent(x1: Double, x2: Double, y1: Double, y2: Double) =
    (e: MouseEvent) =>
      if (e.clientX < x2 && e.clientX > x1 && e.clientY < y2 && e.clientY > y1) {
        cleanEventListener(x1, x2, y1, y2)
        start()
    }

  def start() = {
    import browserStuff._
    import Global._

    dom.window.addEventListener("mousemove", mouseMoveEvent)
    dom.window.addEventListener("touchmove", touchMoveEvent)

    intervalId += dom.window.setInterval(
      () => {
        if (foes.length < 10) Foes.addFoe(foes)
        foes --= foes.filter(f => f.y > browserStuff.height + 20)
      },
      2000
    )

    intervalId += dom.window.setInterval(
      () => {
        foes.foreach(_.foesMoves())
      },
      50
    )

    intervalId += dom.window.setInterval(
      () => {
        foes.foreach(_.turn)
      },
      70
    )

    intervalId += dom.window.setInterval(
      () => {
        Global.updateBulletPosition
      },
      50
    )

    intervalId += dom.window.setInterval(
      () => {
        ship.fire()
        if (Utils.checkDamage(ship, foes)) {
          end()
        }
      },
      100
    )

    intervalId += dom.window.setInterval(
      () => {
        foes.foreach(_.fire())
      },
      150
    )

    intervalId += dom.window.setInterval(
      () => {
        clearCtx
        Global.drawHealthBar(ship, ctx)
        Global.drawEnergy(ship, ctx)
        Global.drawScore(ctx)
        Global.allBullets.foreach(_.draw)
        ship.drawShip()
        foes.foreach(_.drawShip())
      },
      10
    )
  }

  def end() = {
    intervalId.foreach(dom.window.clearInterval)
    import browserStuff._
    ctx.beginPath
    ctx.fillStyle = "#FFFFFF"
    val startingPoint = height * 0.4
    writeTextInMiddle("Ouch :(", "#000000", startingPoint)
    writeTextInMiddle("Another go?", "#000000", startingPoint + 55)
    textInBox("..yes?", "#000000", "#FFFFFF", )
  }

  def textInBox(msg: String,
                x1: Double,
                x2: Double,
                y1: Double,
                y2: Double,
                margin: Double,
                backgroundColor: String,
                textColor: String,
                y: Double) = {
    ctx.font = "50px Verdana"
    ctx.beginPath()
    ctx.fillStyle = backgroundColor

    ctx.beginPath()
    ctx.fillStyle = "#000000"
    ctx.fillRect(x1, y1, x2 - x1, y2 - y1)
    dom.window.addEventListener("click", startAgainEvent(x1, y2, x2, y2))
    writeTextInMiddle(msg, textColor, y + 50 - margin)
  }

  def writeTextInMiddle(msg: String, color: String, y: Double) = {
    import browserStuff._
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.font = "50px Verdana"
    val x = (width - ctx.measureText(msg).width) / 2.0
    ctx.fillText(msg, x, y)
  }

  def main(args: Array[String]): Unit = {
    start()
  }

}
