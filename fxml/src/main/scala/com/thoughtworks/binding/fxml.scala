package com.thoughtworks.binding

import java.beans.{BeanInfo, Introspector, PropertyDescriptor}
import javafx.application.Platform
import javafx.beans.DefaultProperty
import javafx.fxml.JavaFXBuilderFactory
import javax.swing.SwingUtilities

import com.thoughtworks.binding.Binding.{BindingSeq, Constants, MultiMountPoint}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.{TypecheckException, whitebox}
import scala.language.experimental.macros
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor._

import scala.collection.immutable.Queue

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class fxml extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro fxml.Macros.macroTransform
}

object fxml {

  object Runtime {

    private[Runtime] type ComponentOrComponentSeq = Any

    private[Runtime] type RawValue = Any

    @compileTimeOnly("For internal usage only")
    final class Attributes(keyValuPairs: (String, RawValue)*)

    @compileTimeOnly("For internal usage only")
    final class DefaultProperties(children: ComponentOrComponentSeq*)

    @compileTimeOnly("For internal usage only")
    final class Properties(name: String, attributes: Attributes, children: ComponentOrComponentSeq*)

    final case class TypeCoercion(value: String) extends AnyVal

    object TypeCoercion {
      // TODO: implicit conversions from String to Int, Color, etc
    }

    final class EmptyText(val value: String) extends AnyVal

    final def toBindingSeqBinding[A](bindingSeqBinding: Binding[BindingSeq[A]]) = bindingSeqBinding

    final def toBindingSeqBinding[A](binding: Binding[A], dummy: Unit = ()) = {
      Binding.Constant(Constants(()).mapBinding(_ => binding))
    }

    final def toBindingSeq[A](bindingSeqBinding: Binding[BindingSeq[A]]) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => bindingSeq
        case _ => Constants(()).flatMapBinding(_ => bindingSeqBinding)
      }
    }

    final def toBindingSeq[A](bindingSeqBinding: Binding[A], dummy: Unit = ()) = {
      bindingSeqBinding match {
        case Binding.Constant(bindingSeq) => Constants(bindingSeq)
        case _ => Constants(()).mapBinding(_ => bindingSeqBinding)
      }
    }

    final def toBindingSeq[A](bindingSeq: BindingSeq[A]) = {
      bindingSeq
    }

    final def toBindingSeq[A](bindingSeq: A) = {
      Constants(bindingSeq)
    }

    // This macro does not work if it uses a whitebox Context.
    // I have to use deprecated `scala.reflect.macros.Context` instead.
    def autoBind(c: scala.reflect.macros.Context): c.Expr[Any] = {
      import c.universe._
      c.Expr[Any](
        c.macroApplication match {
          case q"$parent.$macroName" =>
            q"$parent.${newTermName(s"${macroName.decodedName}$$binding")}.bind"
          case Ident(macroName) =>
            q"${newTermName(s"${macroName.decodedName}$$binding")}.bind"
        }
      )
    }

    object EmptyConstructor {
      def apply[A](a: => A) = new EmptyConstructor(a _)
      implicit def emptyConstructor[A]: EmptyConstructor[A] = macro Macros.emptyConstructor[A]
    }

    final class EmptyConstructor[A](val f: () => A) extends AnyVal {
      def apply() = f()
    }

    final class JavaMapBuilder[Key, Value](val self: java.util.Map[Key, Value])
        extends Builder[java.util.Map[Key, Value]] {
      // TODO: add map fields
    }

    final class JavaBeanBuilder[A](val self: A) extends Builder[A] {

      def bindProperty(propertyName: String, namedValueMap: Map[String, Any], valueSeq: Seq[Any]): Unit =
        macro Macros.javaBeanBindProperty

      def bindDefaultProperty(values: Any*): Unit = macro Macros.javaBeanBindDefaultProperty

      def build: A = self

    }

    final class JavaFXBuilder[A](val self: javafx.util.Builder[A]) {

      def build = self.build

    }

    private[Runtime] sealed trait LowLowPriorityBuilder {

      // FIXME: Use custom builder instead
      implicit final def javafxBuilder[A](implicit manifest: Manifest[A]): JavaFXBuilder[A] = {
        new JavaFXBuilder[A](
          new JavaFXBuilderFactory().getBuilder(manifest.runtimeClass).asInstanceOf[javafx.util.Builder[A]])
      }

    }

    private[Runtime] sealed trait LowPriorityBuilder extends LowLowPriorityBuilder {

      implicit final def javaBeanBuilder[A](implicit constructor: EmptyConstructor[A]): JavaBeanBuilder[A] = {
        new JavaBeanBuilder[A](constructor())
      }

    }

    object Builder extends LowPriorityBuilder {

      implicit def JavaMapBuilder[Key, Value, M <: java.util.Map[Key, Value]](
          implicit constructor: EmptyConstructor[M]): JavaMapBuilder[Key, Value] = {
        new JavaMapBuilder[Key, Value](constructor())
      }

      def apply[Value](implicit builder: Builder[Value]): builder.type = builder

    }

    trait Builder[Value]

    final class JavaListMountPoint[A](javaList: java.util.List[A])(bindingSeq: BindingSeq[A])
        extends MultiMountPoint[A](bindingSeq) {

      import collection.JavaConverters._

      override protected def set(children: Seq[A]): Unit = {
        javaList.clear()
        javaList.addAll(children.asJava)
      }

      override protected def splice(from: Int, that: GenSeq[A], replaced: Int): Unit = {
        val i = javaList.listIterator(from)
        for (_ <- 0 until replaced) {
          i.next()
          i.remove()
        }
        for (newElement <- that) {
          i.add(newElement)
        }
      }
    }

  }

  import com.thoughtworks.binding.fxml.Runtime._

  private object Macros {

    private def initializeJavaFx() = {
      if (!Platform.isFxApplicationThread) {
        val lock = new AnyRef
        @volatile var initialized = false
        lock.synchronized {
          SwingUtilities.invokeLater(new Runnable {
            override def run(): Unit = {
              new javafx.embed.swing.JFXPanel
              Platform.runLater(new Runnable() {
                override def run(): Unit = {
                  lock.synchronized {
                    initialized = true
                    lock.notify()
                  }
                }
              })
            }
          })
          while (!initialized) {
            lock.wait()
          }
        }
      }
    }

    initializeJavaFx()

    private[Macros] val Spaces = """\s*""".r

    private[Macros] val ExpressionBinding = """(?s)\$\{(.*)\}\s*""".r

    private[Macros] val VariableResolution = """(?s)\$(.*)""".r

    private[Macros] val EscapeSequences = """(?s)\\(.*)""".r

    private[Macros] val ResourceResolution = """(?s)%(.*)""".r

    private[Macros] val LocationResolution = """(?s)@(.*)""".r

  }

  @bundle
  private[binding] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._
    import Macros._

    private def bindPropertyFromDescriptor(parentBean: Tree,
                                           descriptor: PropertyDescriptor,
                                           namedValues: Seq[(String, Tree)],
                                           values: Seq[Tree]): Tree = {
      def ensureNoNamedValue(): Unit = {
        namedValues match {
          case Seq() =>
          case (_, headValue) +: _ =>
            c.error(headValue.pos, "Only read-only map properties can contain attributes")
        }
      }
      def ensureNoChildElements(): Unit = {
        values match {
          case Seq() =>
          case Seq(emptyText) if emptyText.tpe <:< typeOf[Binding.Constant[EmptyText]] =>
          case headValue +: _ =>
            println(show(headValue.tpe))
            c.error(headValue.pos, "Read-only map properties cannot contain child elements")
        }
      }
      descriptor.getWriteMethod match {
        case null if classOf[java.util.Map[_, _]].isAssignableFrom(descriptor.getPropertyType) =>
          ensureNoChildElements()
          val map = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          q"""..${for ((key, value) <- namedValues) yield {
            val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$$key$$mountPoint"))
            atPos(value.pos) {
              q"""
                val $mountPointName = _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]($map.put($key, $value))
                $mountPointName.bind
              """
            }
          }}"""
        case null if classOf[java.util.List[_]].isAssignableFrom(descriptor.getPropertyType) =>
          ensureNoNamedValue()
          def list = q"$parentBean.${TermName(descriptor.getReadMethod.getName)}"
          values match {
            case Seq() =>
              q"()"
            case Seq(name) =>
              val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$mountPoint"))
              q"""
                val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeq($name)
                )
                $mountPointName.bind
              """
            case _ =>
              val valueBindings = for (name <- values) yield {
                q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
              }
              val mountPointName = TermName(c.freshName(s"${descriptor.getName}$$mountPoint"))
              q"""
                val $mountPointName = new _root_.com.thoughtworks.binding.fxml.Runtime.JavaListMountPoint(
                  $list
                )(
                  _root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _)
                )
                $mountPointName.bind
              """
          }
        case writeMethod =>
          ensureNoNamedValue()
          val Seq(value) = values
          atPos(value.pos) {
            if (value.tpe <:< typeOf[Binding.Constant[_]]) {
              q"$parentBean.${TermName(writeMethod.getName)}($value.get)"
            } else {
              val monadicBody = q"$parentBean.${TermName(writeMethod.getName)}($value.bind)"
              q"_root_.com.thoughtworks.binding.Binding[_root_.scala.Unit]($monadicBody).bind"
            }
          }
      }
    }

    def javaBeanBindProperty(propertyName: Tree, namedValueMap: Tree, valueSeq: Tree): Tree = {
      // TODO: named value for read-only map
      val parentBean = q"${c.prefix.tree}.self"
      val beanClass = Class.forName(c.prefix.tree.tpe.widen.typeArgs.head.typeSymbol.fullName)
      val q"$seqApply[..$seqType](..$values)" = valueSeq
      val q"$mapApply[..$mapType](..$mapValues)" = namedValueMap
      val namedValues = for (q"(${Literal(Constant(name: String))}, $value)" <- mapValues) yield {
        name -> value
      }
      val beanInfo = Introspector.getBeanInfo(beanClass)
      val Literal(Constant(propertyNameString: String)) = propertyName
      val descriptorOption = beanInfo.getPropertyDescriptors.find(_.getName == propertyNameString)
      descriptorOption match {
        case None =>
          c.error(propertyName.pos, s"property $propertyNameString is not found")
          q"???"
        case Some(descriptor) =>
          bindPropertyFromDescriptor(parentBean, descriptor, namedValues, values)
      }
    }

    def javaBeanBindDefaultProperty(values: Tree*): Tree = {
      val parentBean = q"${c.prefix.tree}.self"
      val beanClass = Class.forName(c.prefix.tree.tpe.widen.typeArgs.head.typeSymbol.fullName)
      val beanInfo = Introspector.getBeanInfo(beanClass, classOf[AnyRef], Introspector.USE_ALL_BEANINFO)

      def findDefaultProperty: Option[PropertyDescriptor] = {
        beanInfo.getDefaultPropertyIndex match {
          case -1 =>
            beanClass.getAnnotation(classOf[DefaultProperty]) match {
              case null =>
                None
              case defaultProperty =>
                beanInfo.getPropertyDescriptors.find(_.getName == defaultProperty.value)
            }
          case i =>
            Some(beanInfo.getPropertyDescriptors.apply(i))
        }
      }
      findDefaultProperty match {
        case None =>
          c.error(parentBean.pos, s"Default property for ${beanClass.getCanonicalName} is not found.")
          q"???"
        case Some(descriptor) =>
          bindPropertyFromDescriptor(parentBean, descriptor, Seq.empty, values)
      }
    }

    def macroTransform(annottees: Tree*): Tree = {
      val transformer = new ComprehensionTransformer {
        private def transformChildren(children: List[Tree]) = {
          @tailrec
          def loop(children: List[Tree],
                   accumulatedDefinitions: Queue[Tree],
                   accumulatedPropertyBindings: Queue[(String, Position, Seq[(String, Tree)], Seq[Tree])],
                   accumulatedDefaultBindings: Queue[Tree])
            : (Queue[Tree], Queue[(String, Position, Seq[(String, Tree)], Seq[Tree])], Queue[Tree]) = {
            children match {
              case Nil =>
                (accumulatedDefinitions, accumulatedPropertyBindings, accumulatedDefaultBindings)
              case head :: tail =>
                head match {
                  // TODO: other cases
                  case transformImport.extract(transformedImport) =>
                    loop(
                      tail,
                      accumulatedDefinitions.enqueue(transformedImport),
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings
                    )
                  case Text(Macros.Spaces()) =>
                    loop(
                      tail,
                      accumulatedDefinitions,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings
                    )
                  case transformNode.extract(defs, transformedValue) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ defs,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue(transformedValue)
                    )
                  case tree @ Elem(UnprefixedName(propertyName),
                                   attributes,
                                   _,
                                   transformNodeSeq.extract(defs, transformedValues))
                      if propertyName.charAt(0).isLower =>
                    val (attributeDefs, transformedAttributes) = transformAttributes(attributes)
                    loop(
                      tail,
                      accumulatedDefinitions ++ attributeDefs ++ defs,
                      accumulatedPropertyBindings.enqueue(
                        (propertyName, tree.pos, transformedAttributes, transformedValues)),
                      accumulatedDefaultBindings
                    )
                  case tree =>
                    loop(
                      tail,
                      accumulatedDefinitions,
                      accumulatedPropertyBindings,
                      accumulatedDefaultBindings.enqueue(super.transform(tree))
                    )
                }
            }
          }
          loop(children, Queue.empty, Queue.empty, Queue.empty)
        }

        private def singleEmptyText(value: String) = {
          val bindingName = TermName(c.freshName("emptyText"))
          q"def $bindingName = _root_.com.thoughtworks.binding.Binding.Constant(new _root_.com.thoughtworks.binding.fxml.Runtime.EmptyText($value))" -> q"$bindingName"
        }

        private def transformImport: PartialFunction[Tree, Tree] = {
          case tree @ ProcInstr("import", proctext) =>
            atPos(tree.pos) {
              c.parse(raw"""import $proctext""") match {
                case q"import $parent.*" => q"import $parent._"
                case i => i
              }
            }
        }

        private def transformAttributeValue(attributeValue: Tree): (Seq[Tree], Tree) = {
          attributeValue match {
            case TextAttribute(textValue) =>
              textValue match {
                case ExpressionBinding(code) =>
                  c.parse(code)
                  ???
                case VariableResolution(code) =>
                  c.parse(code)
                  ???
                case ResourceResolution(resource) =>
                  ???
                case LocationResolution(location) =>
                  Nil -> atPos(attributeValue.pos)(q"""this.getClass.getResource($location).toString()""")
                case EscapeSequences(rawText) =>
                  Nil -> atPos(attributeValue.pos)(q"$rawText")
                case _ =>
                  Nil -> atPos(attributeValue.pos)(q"$textValue")
              }
            case _ =>
              ???
          }
        }

        private def transformAttributes(attributes: List[(QName, Tree)]): (Seq[Tree], Seq[(String, Tree)]) = {
          @tailrec
          def loop(attributes: List[(QName, Tree)],
                   accumulatedDefinitions: Queue[Tree],
                   accumulatedPairs: Queue[(String, Tree)]): (Queue[Tree], Queue[(String, Tree)]) = {
            attributes match {
              case Nil =>
                (accumulatedDefinitions, accumulatedPairs)
              case (key, value) :: tail =>
                val (attributeDefinitions, transformedAttributeValue) = transformAttributeValue(value)
                key match {
                  case UnprefixedName(attributeName) =>
                    loop(
                      tail,
                      accumulatedDefinitions ++ attributeDefinitions,
                      accumulatedPairs.enqueue((attributeName, transformedAttributeValue))
                    )
                  case _ =>
                    c.error(value.pos, "attributes should not be prefixed")
                    loop(tail, accumulatedDefinitions, accumulatedPairs)
                }
            }
          }
          loop(attributes, Queue.empty, Queue.empty)
        }

        private def transformNodeSeq: PartialFunction[List[Tree], (Seq[Tree], Seq[Tree])] = {
          case Seq() =>
            val (defs, binding) = singleEmptyText("")
            Seq(defs) -> Seq(binding)
          case Seq(tree @ Text(singleText @ Macros.Spaces())) =>
            val (defs, binding) = singleEmptyText(singleText)
            Seq(defs) -> Seq(binding)
          case children =>
            @tailrec
            def loop(nestedChildren: List[Tree],
                     accumulatedDefinitions: Queue[Tree],
                     accumulatedBindings: Queue[Tree]): (Queue[Tree], Queue[Tree]) = {
              nestedChildren match {
                case Nil =>
                  (accumulatedDefinitions, accumulatedBindings)
                case head :: tail =>
                  head match {
                    // TODO: other cases
                    case transformImport.extract(transformedImport) =>
                      loop(tail, accumulatedDefinitions.enqueue(transformedImport), accumulatedBindings)
                    case Text(Macros.Spaces()) =>
                      loop(tail, accumulatedDefinitions, accumulatedBindings)
                    case transformNode.extract(defs, transformedValue) =>
                      loop(tail, accumulatedDefinitions ++ defs, accumulatedBindings.enqueue(transformedValue))
                    case tree =>
                      loop(
                        tail,
                        accumulatedDefinitions,
                        accumulatedBindings.enqueue(super.transform(tree))
                      )

                  }
              }
            }
            loop(children, Queue.empty, Queue.empty)
        }

        private def transformNode: PartialFunction[Tree, (Seq[Tree], Tree)] = {
          // TODO: static property
          case tree @ Text(data) =>
            Nil -> atPos(tree.pos) {
              q"_root_.com.thoughtworks.binding.Binding.Constant($data)"
            }
          case tree @ Elem(UnprefixedName(className), attributes, _, children) if className.charAt(0).isUpper =>
            // TODO: create new instance

            // TODO: <fx:include> (Read external files)
            // TODO: convert fx:value, fx:constant, <fx:reference> and <fx:copy> to @fxml val

            // Type Coercion
            // FXML uses "type coercion" to convert property values to the appropriate type as needed. Type coercion is required because the only data types supported by XML are elements, text, and attributes (whose values are also text). However, Java supports a number of different data types including built-in primitive value types as well as extensible reference types.
            // The FXML loader uses the coerce() method of BeanAdapter to perform any required type conversions. This method is capable of performing basic primitive type conversions such as String to boolean or int to double, and will also convert String to Class or String to Enum. Additional conversions can be implemented by defining a static valueOf() method on the target type.
            //
            // 不要支持Type Coercion、Location Resolution、Resource Resolution、Variable Resolution、Escape Sequences、Expression Binding，要求用户改用花括号{}以提供类型安全的代码

            // fx:define 生成valDefs

            // 不支持 @FXML

            // TODO: If an element represents a type that already implements Map (such as java.util.HashMap), it is not wrapped and its get() and put() methods are invoked directly. For example, the following FXML creates an instance of HashMap and sets its "foo" and "bar" values to "123" and "456", respectively:

            val (fxAttributes, otherAttributes) = attributes.partition {
              case (PrefixedName("fx", _), _) => true
              case _ => false
            }

            val fxAttributeMap = fxAttributes.view.map {
              case (PrefixedName("fx", key), value) => key -> value
            }.toMap

            val fxIdOption = fxAttributeMap.get("id").map {
              case Text(nonEmptyId) =>
                nonEmptyId
              case EmptyAttribute() =>
                c.error(tree.pos, "fx:id must not be empty.")
                "<error>"
            }
            (fxAttributeMap.get("factory"), fxAttributeMap.get("value")) match {
              case (Some(_), Some(_)) =>
                c.error(tree.pos, "fx:factory and fx:value must not be present on the same element.")
                Nil -> q"???"
              case (None, None) =>


                // TODO: attributes
                //
                //            val attributeMountPoints = for {
                //              attribute <- attributes
                //            } yield {
                //              val (attributeAccess, value) = attribute match {
                //                case Left((key, value)) =>
                //                  val keyName = TermName(key)
                //                  q"""new _root_.com.thoughtworks.binding.fxml.Runtime.StaticBeanAdapter($elementName).$keyName""" -> value
                //                case Right((pre, key, value)) =>
                //                  key.split(':').foldLeft(q"""new _root_.com.thoughtworks.binding.fxml.Runtime.StaticBeanAdapter($elementName).${TermName(pre)}""") { (prefixExpr, propertyName) =>
                //                    q"""new _root_.com.thoughtworks.binding.Runtime.StaticBeanAdapter($prefixExpr).${TermName(propertyName)}"""
                //                  } -> value
                //              }
                //              atPos(value.pos) {
                //                val assignName = TermName(c.freshName("assignAttribute"))
                //                val newValueName = TermName(c.freshName("newValue"))
                //                q"""
                //                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                //                    _root_.com.thoughtworks.binding.Binding,
                //                    _root_.scala.Unit
                //                  ](
                //                    _root_.com.thoughtworks.binding.Binding.apply[_root_.scala.Unit]({
                //                      val $newValueName = ${transform(value)}
                //                      @_root_.scala.inline def $assignName() = {
                //                        if ($attributeAccess != $newValueName) {
                //                          $attributeAccess = $newValueName
                //                        }
                //                      }
                //                      $assignName()
                //                    })
                //                  )
                //                """
                //              }
                //            }
                //

                val elementName = fxIdOption match {
                  case None =>
                    TermName(c.freshName(className))
                  case Some(id) =>
                    TermName(id)
                }
                val (childrenDefinitions, childrenProperties, defaultProperties) = transformChildren(children)
//                val builderBuilderName = TermName(c.freshName("builderBuilder"))
//                val bindingName = TermName(s"${elementName.decodedName}$$binding")
//
                def bindingDef = {
//                  val bindProperties = for ((propertyName, pos, namedValues, values) <- childrenProperties) yield {
//                    val namedTuples = for ((name, value) <- namedValues) yield {
//                      q"($name, $value)"
//                    }
//                    atPos(pos) {
//                      q"""$builderBuilderName.bindProperty($propertyName, _root_.scala.Predef.Map(..$namedTuples), _root_.scala.Seq(..$values))"""
//                    }
//                  }
//                  val bindDefaultProperties = if (defaultProperties.isEmpty) {
//                    Nil
//                  } else {
//                    List(atPos(tree.pos) {
//                      q"$builderBuilderName.bindDefaultProperty(..$defaultProperties)"
//                    })
//                  }
                  atPos(tree.pos) {
                    q"""
                      val $bindingName = {
                        val $builderBuilderName = _root_.com.thoughtworks.binding.fxml.Runtime.Builder.apply[${TypeName(
                      className)}]
                        def $elementName = $builderBuilderName.self
                        _root_.com.thoughtworks.binding.Binding.apply({
                          ..$bindProperties
                          ..$bindDefaultProperties
                          $builderBuilderName.build
                        })
                      }
                    """
                  }
                }


                val defs = if (fxIdOption.isDefined) {
                  val autoBindDef = atPos(tree.pos) {
                    q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                  }
                  childrenDefinitions.enqueue(bindingDef).enqueue(autoBindDef)
                } else {
                  childrenDefinitions.enqueue(bindingDef)
                }

                defs -> atPos(tree.pos)(q"$bindingName")
              case (Some(EmptyAttribute()), None) =>
                c.error(tree.pos, "fx:factory must not be empty.")
                Nil -> q"???"
              case (Some(Text(fxFactory)), None) =>
                transformChildren(children) match {
                  case (childrenDefinitions, Queue(), defaultProperties) =>
                    val elementName = fxIdOption match {
                      case None =>
                        TermName(c.freshName(className))
                      case Some(id) =>
                        TermName(id)
                    }
                    val bindingName = TermName(s"${elementName.decodedName}$$binding")
                    def bindingDef = {
                      val factoryArgumentNames = for (i <- 0 until defaultProperties.length) yield {
                        TermName(c.freshName(s"fxFactoryArgument$i"))
                      }
                      val factoryArguments = for (name <- factoryArgumentNames) yield {
                        q"val $name = $EmptyTree"
                      }
                      // TODO: Support more than 12 parameters by generate more sophisticated code
                      val applyN = TermName(s"apply${defaultProperties.length}")
                      q"""
                        val $bindingName = _root_.com.thoughtworks.binding.Binding.BindingInstances.$applyN(..$defaultProperties)({ ..$factoryArguments =>
                          ${TermName(className)}.${TermName(fxFactory)}(..$factoryArgumentNames)
                        })
                      """
                    }
                    val defs = if (fxIdOption.isDefined) {
                      val autoBindDef = atPos(tree.pos) {
                        q"def $elementName: _root_.scala.Any = macro _root_.com.thoughtworks.binding.fxml.Runtime.autoBind"
                      }
                      childrenDefinitions.enqueue(bindingDef).enqueue(autoBindDef)
                    } else {
                      childrenDefinitions.enqueue(bindingDef)
                    }
                    defs -> atPos(tree.pos)(q"$bindingName")
                  case (_, (_, pos, _, _) +: _, _) =>
                    c.error(pos, "fx:factory must not contain named property")
                    Nil -> q"???"
                }
              case (None, Some(TextAttribute(fxValue))) =>
                fxIdOption match {
                  case None =>
                    Nil -> atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(className)}.valueOf($fxValue))"
                    }
                  case Some(fxId) =>
                    val idDef = atPos(tree.pos) {
                      q"val ${TermName(fxId)} = ${TermName(className)}.valueOf($fxValue)"
                    }
                    Queue(idDef) -> atPos(tree.pos) {
                      q"_root_.com.thoughtworks.binding.Binding.Constant(${TermName(fxId)})"
                    }
                }
            }

          case tree @ NodeBuffer(transformNodeSeq.extract(defs, values)) =>
            defs -> atPos(tree.pos) {
              values match {
                case Seq() =>
                  q"_root_.com.thoughtworks.binding.Binding.Constant(())"
                case Seq(value) =>
                  value
                case _ =>
                  val valueBindings = for (name <- values) yield {
                    q"_root_.com.thoughtworks.binding.fxml.Runtime.toBindingSeqBinding($name)"
                  }
                  q"_root_.com.thoughtworks.binding.Binding.Constant(_root_.com.thoughtworks.binding.Binding.Constants(..$valueBindings).flatMapBinding(_root_.scala.Predef.locally _))"
              }
            }
          case tree @ Elem(PrefixedName("fx", "include"), attributes, _, children) =>
            c.error(tree.pos, "fx:include is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "reference"), attributes, _, children) =>
            c.error(tree.pos, "fx:reference is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "copy"), attributes, _, children) =>
            c.error(tree.pos, "fx:copy is not supported yet.")
            Nil -> q"???"
          case tree @ Elem(PrefixedName("fx", "root"), attributes, _, children) =>
            c.error(tree.pos, "fx:root is not supported yet.")
            Nil -> q"???"
        }

        override def transform(tree: Tree): Tree = {
          tree match {
            case transformNode.extract(defs, transformedValue) =>
              val xmlScopeName = TypeName(c.freshName("XmlScope"))
              val rootName = TermName(c.freshName("root"))

              q"""
                final class $xmlScopeName {
                  ..$defs
                  def $rootName = $transformedValue
                }
                (new $xmlScopeName).$rootName.bind
              """
            case _ =>
              super.transform(tree)
          }
        }
      }

      import transformer.transform
//      def transform(tree: Tree): Tree = {
//        val output = transformer.transform(tree)
//        c.info(c.enclosingPosition, show(output), true)
//        output
//      }

      replaceDefBody(
        annottees, { body =>
          q"""
          import _root_.scala.language.experimental.macros
          _root_.com.thoughtworks.binding.Binding.apply(${transform(body)})
        """
        }
      )

    }

    def emptyConstructor[A](implicit weakTypeTag: c.WeakTypeTag[A]) = {
      q"_root_.com.thoughtworks.binding.fxml.Runtime.EmptyConstructor(new ${weakTypeTag.tpe}())"
    }
  }

}
