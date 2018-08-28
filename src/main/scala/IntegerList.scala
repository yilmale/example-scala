
import org.nlogo.api.ScalaConversions._
import org.nlogo.core.Syntax
import org.nlogo.core.Syntax.{ NumberType, ListType, WildcardType, RepeatableType }


import org.nlogo.{ agent, api, core, nvm }
import org.nlogo.api._
import core.Syntax._
import api.ScalaConversions._  // implicits
import org.nlogo.core.AgentKind

import java.io._


class SampleScalaExtension extends DefaultClassManager {
  def load(manager: PrimitiveManager) {
    manager.addPrimitive("first-n-integers", new IntegerList)
    manager.addPrimitive("my-list", MyList)
    manager.addPrimitive("create-red-turtles", CreateRedTurtles)
    manager.addPrimitive("patch-set-up",
      (new patchWithNoGrass_patchModel_base_AbstractBase.MyPatch).generateCommand())
    manager.addPrimitive("prey-action",
      (new PreyActivity.MyPrey).generateCommand())
    manager.addPrimitive("update", SetGlobal)
  }
}

class IntegerList extends Reporter {
  override def getSyntax = Syntax.reporterSyntax(right = List(NumberType), ret = ListType)
  def report(args: Array[Argument], context: Context): AnyRef = {
    val n = try args(0).getIntValue
    catch {
      case e: LogoException =>
        throw new ExtensionException(e.getMessage)
    }
    if (n < 0)
      throw new ExtensionException("input must be positive")
    (0 until n).toLogoList
  }
}

object MyList extends Reporter {
  override def getSyntax = Syntax.reporterSyntax(right = List(WildcardType | RepeatableType), ret = ListType, defaultOption = Some(3))
  def report(args: Array[Argument], context: Context) =
    args.map(_.get).toLogoList

}


  object SetGlobal extends Reporter {
    override def getSyntax = Syntax.reporterSyntax(right = List(NumberType),
      ret = NumberType, defaultOption = Some(1))

    def report(args: Array[Argument], context: Context) = {
      //args.map(_.get).toLogoList
      val a: Double = args(0).getDoubleValue * 2
      a.toLogoObject
    }

  }


object PredatorActivity {
  class Base_FeatureModel {}
  class MyPredator extends Base_FeatureModel with P_MyPred {
    def move()(implicit context: api.Context): Unit ={
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val world = context.getAgent.world.asInstanceOf[agent.World]
      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]

      val r = scala.util.Random
      t.heading((t.heading + (r.nextDouble()*50)) - (r.nextDouble()*50))
      t.jump(1.0)

      //reduce energy
      var index = world.turtlesOwnIndexOf("ENERGY")
      t.setVariable(index,(t.getVariable(index).asInstanceOf[Double]-1).toLogoObject)
    }
    def reproduce()(implicit context: api.Context): Unit = {}
    def death()(implicit context: api.Context): Unit = {}
    def consume(e: Double)(implicit context: api.Context) : Unit ={
     /* val world = context.getAgent.world.asInstanceOf[agent.World]
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      var index = world.turtlesOwnIndexOf("ENERGY")

      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
      var p = t.getPatchHere

      val pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt" ))


      var trts = p.turtlesHere().iterator()
      while (trts.hasNext()) {
        var b = trts.next()
        if (b.getBreed().printName == "SHEEP")
           pw.println("Found " + b.getBreed().printName)
      }
      pw.close()


      let prey one-of sheep-here                    ; grab a random sheep
      if prey != nobody  [                          ; did we get one?  if so,
      ask prey [ die ]                            ; kill it, and...
      set energy energy + wolf-gain-from-food     ; get energy from eating
      ]
    }*/

  }
  }

  trait P_MyPred {
    def move()(implicit context: api.Context): Unit
    def reproduce()(implicit context: api.Context): Unit
    def death()(implicit context: api.Context): Unit
    def consume(e: Double)(implicit context: api.Context) : Unit

    def generateCommand(): PredatorAction = {
      new PredatorAction
    }
  }

  class PredatorAction extends Command with nvm.CustomAssembled  {
    override def getSyntax = Syntax.commandSyntax(right = List(NumberType, NumberType, CommandBlockType | OptionalType))

    def perform(args: Array[api.Argument], context: api.Context): Unit = {
      implicit val myContext: api.Context = context
      val world = context.getAgent.world.asInstanceOf[agent.World]
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val nvmContext = eContext.nvmContext
    }

    def assemble(a: nvm.AssemblerAssistant): Unit = {
      a.block()
      a.done()
    }
  }
}


object PreyActivity {
  class Base_FeatureModel {}

  class MyPrey extends Base_FeatureModel with P_MyPrey {
    def move()(implicit context: api.Context): Unit = {
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val world = context.getAgent.world.asInstanceOf[agent.World]
      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]

      val r = scala.util.Random
      t.heading((t.heading + (r.nextDouble()*50)) - (r.nextDouble()*50))
      t.jump(1.0)

      //reduce energy
      var index = world.turtlesOwnIndexOf("ENERGY")
      t.setVariable(index,(t.getVariable(index).asInstanceOf[Double]-1).toLogoObject)

    }

    def reproduce()(implicit context: api.Context): Unit = {
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val world = context.getAgent.world.asInstanceOf[agent.World]
      var index = world.turtlesOwnIndexOf("ENERGY")
      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
      val r = scala.util.Random
      var e : Double  = t.getVariable(index).asInstanceOf[Double]
      e = e / 2
      t.setVariable(index,e.toLogoObject)
      var c : org.nlogo.agent.Turtle = t.hatch(t.getBreed())
      eContext.workspace.joinForeverButtons(c)
      c.heading(c.heading + (r.nextDouble()*360))
      c.jump(1.0)
    }
    def death()(implicit context: api.Context): Unit = {
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val world = context.getAgent.world.asInstanceOf[agent.World]
      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
      var index = world.turtlesOwnIndexOf("ENERGY")
      if (t.getVariable(index).asInstanceOf[Double] < 0) t.die()
    }

    def consume(e: Double)(implicit context: api.Context): Unit = {
      val world = context.getAgent.world.asInstanceOf[agent.World]
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      var index = world.turtlesOwnIndexOf("ENERGY")
      var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
      var p = t.getPatchHere
      if (p.pcolor==Color.argbToColor(Color.getRGBByName("green"))) {
        p.setVariable(2, Color.argbToColor(Color.getRGBByName("brown")))
        t.setVariable(index,(t.getVariable(index).asInstanceOf[Double]+e).toLogoObject)
      }

    }
  }

  trait P_MyPrey {
    def move()(implicit context: api.Context): Unit
    def reproduce()(implicit context: api.Context): Unit
    def death()(implicit context: api.Context): Unit
    def consume(e: Double)(implicit context: api.Context) : Unit

    def generateCommand(): PreyAction = {
      new PreyAction
    }


    class PreyAction extends Command with nvm.CustomAssembled {
      override def getSyntax = Syntax.commandSyntax(right = List(NumberType, NumberType, CommandBlockType | OptionalType))

      def perform(args: Array[api.Argument], context: api.Context): Unit = {
        implicit val myContext : api.Context = context
        val world = context.getAgent.world.asInstanceOf[agent.World]
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        val nvmContext = eContext.nvmContext

        var t = eContext.getAgent.asInstanceOf[org.nlogo.agent.Turtle]
        var index = world.turtlesOwnIndexOf("ENERGY")
        val r = scala.util.Random

        move()
        consume(args(0).getDoubleValue)
        death()
        if (r.nextDouble() < (args(1).getDoubleValue*0.01)) reproduce()

      }

      def assemble(a: nvm.AssemblerAssistant): Unit = {
        a.block()
        a.done()
      }
    }

  }

}


object patchWithNoGrass_patchModel_base_AbstractBase {
  trait patchWithGrass_MyPatch {
    def generateCommand(): PatchSetUp = {
      new PatchSetUp
    }
    class PatchSetUp extends Command with nvm.CustomAssembled {
      override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))
      def perform(args: Array[api.Argument], context: api.Context): Unit = {
        var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))
        pw.println("Patch with grass set up")
        val world = context.getAgent.world.asInstanceOf[agent.World]
        val eContext = context.asInstanceOf[nvm.ExtensionContext]
        val nvmContext = eContext.nvmContext
        var patchColor: String = null
        val p: Patch = eContext.getAgent.asInstanceOf[Patch]
        for (x <- AgentVariables.getImplicitPatchVariables(false)) pw.println(x)
        val r = scala.util.Random
        var index = world.patchesOwnIndexOf("COUNTDOWN")

        var grt: Double = args(0).getDoubleValue
        if (r.nextDouble() <= 0.5) {
          pw.println("Patch color was " + p.getVariable(2))
          p.setVariable(2, Color.argbToColor(Color.getRGBByName("green")))
          pw.println("Patch color is now " + p.getVariable(2))
          patchColor = "green"
          p.setVariable(index, grt.toLogoObject)
          pw.println("Grass growth time is " + p.getVariable(index))
        }
        else {
          pw.println("Patch color was " + p.pcolor)
          world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("brown")))
          pw.println("Patch color is now " + p.pcolor)
          patchColor = "brown"
          p.setVariable(index, (r.nextDouble() * grt).toLogoObject)
          pw.println("Grass growth time is " + p.getVariable(index))
        }
        pw.close()
      }
      def assemble(a: nvm.AssemblerAssistant): Unit = {
        a.block()
        a.done()
      }
    }
  }
  class MyPatch extends Base_FeatureModel with patchWithGrass_MyPatch {
    val posx: Int = 10
    val posy: Int = 20
  }
  class Base_FeatureModel
}



object CreateRedTurtles extends api.Command with nvm.CustomAssembled {
  override def getSyntax =
    commandSyntax(right = List(NumberType, CommandBlockType | OptionalType),
      agentClassString = "O---",
      blockAgentClassString = Some("-T--"))

  // only box this once
  private val red = Double.box(15)

  def perform(args: Array[api.Argument], context: api.Context) {
    // the api package have what we need, so we'll often
    // be dropping down to the agent and nvm packages
    val n = args(0).getIntValue
    val world = context.getAgent.world.asInstanceOf[agent.World]
    val eContext = context.asInstanceOf[nvm.ExtensionContext]
    val nvmContext = eContext.nvmContext
    val agents =
      new agent.AgentSetBuilder(AgentKind.Turtle, n)
    for(_ <- 0 until n) {
      val turtle = world.createTurtle(world.turtles)
      turtle.colorDoubleUnchecked(red)
      agents.add(turtle)
      eContext.workspace.joinForeverButtons(turtle)
    }
    // if the optional command block wasn't supplied, then there's not
    // really any point in calling this, but it won't bomb, either
    nvmContext.runExclusiveJob(agents.build(), nvmContext.ip + 1)
    // prim._extern will take care of leaving nvm.Context ip in the right place
  }

  def assemble(a: nvm.AssemblerAssistant) {
    a.block()
    a.done()
  }
}