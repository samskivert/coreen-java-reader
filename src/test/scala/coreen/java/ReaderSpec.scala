//
// $Id$

package coreen.java

import scala.xml.{Elem, PrettyPrinter}

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Tests the Java to name-resolved source translator.
 */
class ReaderSpec extends FlatSpec with ShouldMatchers
{
  val testA = """
    package foo.bar;
    public class TestA {
        public static class A {
            public int value;
        }
        public static class B {
            public void noop () {
            }
        }
        public static void main (String[] args) {
            int av = new A().value;
            B b = new B();
            b.noop();
        }
    }"""

  "Reader" should "handle this code" in {
    val cunit = Reader.process("TestA.java", testA)
    // println(pretty(cunit))

    val pkg = (cunit \ "def").head
    (pkg \ "@name").text should equal("foo.bar")

    val outer = (pkg \ "def").head
    (outer \ "@name").text should equal("TestA")

    val innerA  = (outer \ "def").head
    (innerA \ "@name").text should equal("A")
    (innerA \ "def" \ "@name").text should equal("value")
    // (innerA \ "def" \ "use" \ "@target").text should equal("int")

    val innerB = (outer \ "def").tail.head
    (innerB \ "@name").text should equal("B")
    (innerB \ "def" \ "@name").text should equal("noop")
  }

  val testB = """
  package test;
  @Deprecated
  public interface Foo {
    public String bar ();
  }
  """

  "Reader" should "correctly obtain start pos for annotated class" in {
    val cunit = Reader.process("Foo.java", testB)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    testB.indexOf("@Deprecated") should equal((clazz \ "@bodyStart").text.toInt)
    // println(pretty(cunit))
  }

  val missing = """
  package test;
  import com.nonexist.Bar;
  public class Foo {
    public static void main (String[] args) {
      Bar bar = new Bar();
      System.err.println(bar);
    }
  }
  """

  "Reader" should "partially process code with missing depends" in {
    println("---- compiler output, ignore ----")
    val cunit = Reader.process("Foo.java", missing)
    println("------ end compiler output ------")
    val pkg = (cunit \ "def").head
    (pkg \ "@name").text should equal("test")
    val outer = (pkg \ "def").head
    (outer \ "@name").text should equal("Foo");
    val main = (outer \ "def").head
    val bar = (main \ "def").tail.head
    (bar \ "@sig").text should equal("Bar bar") // no type info for 'Bar'
  }

  val extendsEx = """
  package test;
  public class Foo {
    public class A {
    }
    public class B extends A {
    }
    public interface C {
    }
    public class D implements Foo.C {
    }
  }
  """

  "Reader" should "correctly identify extends and implements" in {
    val cunit = Reader.process("Foo.java", extendsEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    // println(pretty(cunit))
  }

  val thisEx = """
  package test;
  public class Foo {
    public final int foo;
    public final String bar;
    public Foo (int foo, String bar) {
      this.foo = foo;
      this.bar = bar;
    }
  }
  """

  "Reader" should "correctly differntiate this.field and params" in {
    val cunit = Reader.process("Foo.java", thisEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    // println(pretty(cunit))
  }

  val anonEx = """
  package test;
  public class Foo {
    public Foo () {
      Object foo = new Runnable() {
        public void run () {}
      };
    }
  }
  """

  "Reader" should "correctly handle anonymous inner classes" in {
    val cunit = Reader.process("Foo.java", anonEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    // println(pretty(cunit))
  }

  val docEx = """
  package test;
  /** This has some code {@code foo < bar}. And some more code {@code bar > foo}.
   * And a {@literal <literal>}.
   * @author And Mr Author
   * @since 9.99
   */
  public class Foo {
    /** This makes a foo.
     * @param monkey a monkey for your foo.
     */
    public Foo (String monkey) {
      Object foo = new Runnable() {
        public void run () {}
      };
    }
    /** Some funny business: ('\0' to '{@literal \}uFFFF'). */
    public void funnyBiz () {}
  }
  """

  "Reader" should "correctly process Javadoc bits" in {
    val cunit = Reader.process("Foo.java", docEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    (clazz \ "@doc").text should equal("This has some code <code>foo &lt; bar</code>. " +
                                       "And some more code <code>bar &gt; foo</code>.\n" +
                                       "And a &lt;literal&gt;.") // @author and @since stripped
    val ctor = (clazz \ "def").head
    (ctor \ "@doc").text should equal("This makes a foo.<dl>\n" + // TEMP @param hackery
                                      "<dt>monkey</dt><dd>a monkey for your foo.</dd></dl>")
    // println(pretty(cunit))
  }

  val annotationEx = """
  package test;
  @Deprecated
  public class Foo extends Object implements Runnable {
    public Foo () {
      Object foo = new Runnable() {
        public void run () {}
      };
    }
    public void run () {}
  }
  """

  "Reader" should "correctly format class signatures" in {
    val cunit = Reader.process("Foo.java", annotationEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    val sig = (clazz \ "@sig").text
    // TODO: get extends and implements on separate lines
    sig should equal("@Deprecated()\n" +
                     "public class Foo extends Object implements Runnable")
  }

  val nlInDocEx = """
  package test;
  /**
   * Here's some docs with a newline.
   * {@code
   * foo.bar().
   *    .baz();
   * }
   * That's nice.
   */
  public class Foo {}
  """

  "Reader" should "correctly handle newlines in Javadocs" in {
    val cunit = Reader.process("Foo.java", nlInDocEx)
    val pkg = (cunit \ "def").head
    val clazz = (pkg \ "def").head
    val doc = (clazz \ "@doc").text
    doc should equal("Here's some docs with a newline.\n" +
                     "<code>foo.bar().\n" +
                     "   .baz();\n" +
                     "</code>\n" +
                     "That's nice.")
  }

  val paramTypeEx = """
  package test;
  public class Foo {
    public interface A<T> {}
    public class B {}
    public void foo (A<B> test) {}
  }
  """

  "Reader" should "correctly handle parameterized uses" in {
    val cunit = Reader.process("Foo.java", paramTypeEx)
    val pkg = (cunit \ "def").head
    val uses = pkg \\ "use"
    uses.length should equal(2)
    (uses(0) \ "@target").text should equal("test.Foo.A")
    (uses(1) \ "@target").text should equal("test.Foo.B")
  }

  val flavorEx = """
  package test;
  public class Foo {
    public interface A {
      void interfaceMethod ();
    }
    public enum B {}
    public @interface C {
    }
    public abstract class D {
      public abstract void abstractMethod ();
    }
    public int field;
    public Foo () {
      // ctor
    }
    public void method (A param) {
      B local = null;
    }
  }
  """

  "Reader" should "correctly assign flavors" in {
    val cunit = Reader.process("Foo.java", flavorEx)
    val pkg = (cunit \ "def").head
    // println(pretty(cunit))
    val flavs = (pkg \\ "def") map(e => ((e \ "@id").text -> (e \ "@flavor").text)) toMap;
    // println(flavs)
    flavs("test.Foo") should equal("class")
    flavs("test.Foo.A") should equal("interface")
    flavs("test.Foo.A.interfaceMethod()void") should equal("abstract_method")
    flavs("test.Foo.B") should equal("enum")
    flavs("test.Foo.C") should equal("annotation")
    flavs("test.Foo.D") should equal("abstract_class")
    flavs("test.Foo.D.abstractMethod()void") should equal("abstract_method")
    flavs("test.Foo.field") should equal("field")
    flavs("test.Foo.Foo()void") should equal("constructor")
    flavs("test.Foo.method(test.Foo.A)void") should equal("method")
    flavs("test.Foo.method(test.Foo.A)void.param") should equal("param")
    flavs("test.Foo.method(test.Foo.A)void.local") should equal("local")
  }

  val accessEx = """
  package test;
  public class Foo {
    interface A {
      void interfaceMethod ();
    }
    protected class B {
      protected void method () {}
    }
    private int field;
    void method () {}
  }
  """

  "Reader" should "correctly assign access" in {
    val cunit = Reader.process("Foo.java", accessEx)
    val pkg = (cunit \ "def").head
    // println(pretty(cunit))
    val accs = (pkg \\ "def") map(e => ((e \ "@id").text -> (e \ "@access").text)) toMap;
    // println(accs)
    accs("test.Foo") should equal("public")
    accs("test.Foo.A") should equal("default")
    accs("test.Foo.A.interfaceMethod()void") should equal("public")
    accs("test.Foo.B") should equal("protected")
    accs("test.Foo.B.method()void") should equal("protected")
    accs("test.Foo.field") should equal("private")
    accs("test.Foo.method()void") should equal("default")
  }

  val superEx = """
  package test;
  public class Foo {
    interface A extends Runnable {
      void interfaceMethod ();
    }
  }
  """

  "Reader" should "correctly compute super types" in {
    val cunit = Reader.process("Foo.java", superEx)
    val pkg = (cunit \ "def").head
    // println(pretty(cunit))
    val supers = (pkg \\ "def") map(e => ((e \ "@id").text -> (e \ "@supers").text)) toMap;
    // println(supers)
    supers("test.Foo") should equal("java.lang.Object")
    supers("test.Foo.A") should equal("java.lang.Runnable")
  }

  val superMethodEx = """
  package test;
  public class Foo implements Runnable {
    @Override public void run () {}
    @Override public String toString() { return "Foo"; }
  }
  """

  "Reader" should "correctly compute method super types" in {
    val cunit = Reader.process("Foo.java", superMethodEx)
    val pkg = (cunit \ "def").head
    // println(pretty(cunit))
    val supers = (pkg \\ "def") map(e => ((e \ "@id").text -> (e \ "@supers").text)) toMap;
    // println(supers)
    supers("test.Foo.run()void") should equal("java.lang.Runnable.run()void")
    supers("test.Foo.toString()java.lang.String") should equal(
      "java.lang.Object.toString()java.lang.String")
  }

  val paramSuperEx = """
  package test;
  public class Foo {
    interface A extends Comparable<A> {
      void interfaceMethod ();
    }
  }
  """

  "Reader" should "correctly handle parameterized super types" in {
    val cunit = Reader.process("Foo.java", paramSuperEx)
    val pkg = (cunit \ "def").head
    val supers = (pkg \\ "def") map(e => ((e \ "@id").text -> (e \ "@supers").text)) toMap;
    supers("test.Foo") should equal("java.lang.Object")
    supers("test.Foo.A") should equal("java.lang.Comparable")
  }

  protected def pretty (cunit :Elem) = new PrettyPrinter(999, 2).format(cunit)
}
