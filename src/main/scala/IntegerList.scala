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
    manager.addPrimitive("patch-set-up", PatchSetUp)
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

object PatchSetUp extends Command with nvm.CustomAssembled {
  override def getSyntax = Syntax.commandSyntax(right = List(CommandBlockType|OptionalType))
  def perform(args: Array[api.Argument], context: api.Context): Unit = {
    val world = context.getAgent.world.asInstanceOf[agent.World]
    val eContext = context.asInstanceOf[nvm.ExtensionContext]
    val nvmContext = eContext.nvmContext

    val p : Patch = eContext.getAgent.asInstanceOf[Patch]
    val r = scala.util.Random
    if (r.nextDouble() <= 0.5)
      world.patchChangedColorAt(p.id.asInstanceOf[Int],Color.argbToColor(Color.getRGBByName("green")))
    else
      world.patchChangedColorAt(p.id.asInstanceOf[Int],Color.argbToColor(Color.getRGBByName("brown")))



    var pw = new PrintWriter(new File("/Users/yilmaz/IdeaProjects/example-scala/test.txt" ))
    pw.println(p.getVariable(5))
    p.setVariable(5,Some(20.0))
    pw.println()
    pw.println(p.getVariable(5))
    pw.println()
    var index = world.patchesOwnIndexOf("COUNTDOWN")
    pw.println("Index is " + index)
    pw.close()

  }

  def assemble(a: nvm.AssemblerAssistant) {
    a.block()
    a.done()
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