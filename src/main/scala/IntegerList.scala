import org.nlogo.api._
import org.nlogo.api.ScalaConversions._
import org.nlogo.core.Syntax
import org.nlogo.core.Syntax.{ NumberType, ListType, WildcardType, RepeatableType }


class SampleScalaExtension extends DefaultClassManager {
  def load(manager: PrimitiveManager) {
    manager.addPrimitive("first-n-integers", new IntegerList)
    manager.addPrimitive("my-list", MyList)
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