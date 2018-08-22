import org.nlogo.api._
import org.nlogo.api.ScalaConversions._
import org.nlogo.core.Syntax
import org.nlogo.core.Syntax.{ NumberType, ListType, WildcardType, RepeatableType }


import org.nlogo.{ agent, api, core, nvm }
import core.Syntax._
import api.ScalaConversions._  // implicits
import org.nlogo.core.AgentKind

import java.io._


class SampleScalaExtension extends DefaultClassManager {
  def load(manager: PrimitiveManager) {
    manager.addPrimitive("first-n-integers", new IntegerList)
    manager.addPrimitive("my-list", MyList)
    manager.addPrimitive("create-red-turtles", CreateRedTurtles)
    manager.addPrimitive("patch-set-up", (new MyPatch).generateCommand())
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


class BasePatch {

  class PatchSetUp extends Command with nvm.CustomAssembled {
    override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))

    def perform(args: Array[api.Argument], context: api.Context): Unit = {
      var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))

      pw.println("Base patch set up")

      pw.close()
    }

    def assemble(a: nvm.AssemblerAssistant) {
      a.block()
      a.done()
    }
  }

}

trait PatchWithNoGrass {
  class PatchSetUp extends Command with nvm.CustomAssembled {
    override def getSyntax = Syntax.commandSyntax(right = List(NumberType, CommandBlockType | OptionalType))

    def perform(args: Array[api.Argument], context: api.Context): Unit = {
      var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt"))

      pw.println("Patch with grass no grass set up")

      val world = context.getAgent.world.asInstanceOf[agent.World]
      val eContext = context.asInstanceOf[nvm.ExtensionContext]
      val nvmContext = eContext.nvmContext
      val p: Patch = eContext.getAgent.asInstanceOf[Patch]
      world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("green")))
    }

    def assemble(a: nvm.AssemblerAssistant) {
      a.block()
      a.done()
    }
  }
}

trait PatchWithGrass {

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
        val r = scala.util.Random
        var index = world.patchesOwnIndexOf("COUNTDOWN")
        var grt: Double = args(0).getDoubleValue
        if (r.nextDouble() <= 0.5) {
          world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("green")))
          patchColor = "green"
          p.setVariable(index, grt.toLogoObject)
        }
        else {
          world.patchChangedColorAt(p.id.asInstanceOf[Int], Color.argbToColor(Color.getRGBByName("brown")))
          patchColor = "brown"
          p.setVariable(index, (r.nextDouble() * grt).toLogoObject)
        }

        pw.close()
      }

      def assemble(a: nvm.AssemblerAssistant) {
        a.block()
        a.done()
      }
    }
}


class MyPatch extends BasePatch with PatchWithNoGrass {
  def generateCommand(): PatchSetUp = {
    new PatchSetUp
  }
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