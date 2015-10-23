# Functional Reactive UI framework in Scala

With this article and experience, I want to prove that an application wich have a lot of sources of actions, can have state represented by immutable datastructure.

### Collections
Basically we start with a simple collection. Each collection has a scan method. In this case we use the scanLeft method,
because we want to fold up the collection which is a stream of elements, from the left to the right. This is the natural flow by time.

The scanLeft accumulate the actual item with the previously accumulated value and emits this. At the first time this is the zero element.

```scala
object Basics extends App {
  val list = List("A", "B", "C", "D")

  val result = list.scanLeft("Z") { (accu, item) =>
    accu + item
  }

  result.foreach { model =>
    println(model)
  }
  println(result)
}
```

Output is:  
Z  
ZA  
ZAB  
ZABC  
ZABCD  

Substitution:
1. it emits the zero element: Z
2. it accumulates the previous element (Z) with the actual element which is the first element in the collection (A) and emits that: ZA
3. takes the last accumulated (same as emitted) element and the next element in the collection (B), accumulate them and emits that: ZAB
4. continues this till the last element of the collection...

You can see, we always can create a next element by the last element.
Nothing complex, step further, and do the same with

### RxJava streams
within that, the Scala version of that: `rxscala`

These streams are very similars to the collection, two main differences.
- **The collections are strict** which means it is evaluated at creation time
- **Pull based** means the consumer side decide when an element is needed. In the example the consumer is foreach function.
It pulls out the elements from the collection.

Instead of these the streams are:
- **Lazy**: there is no element in the stream at creation time.
- **Push based**: Only a the producer can decide to emitting a value. In this case, the foreach function is also a consumer,
but will run only if the producer emits (pushes) an item. This is why it is lazy.

```
object Stream extends App{
  val s = Subject[String]()

  val result = s.scan("Z"){ (accu, item) =>
    accu + item
  }.foreach { model =>
    println(model)
  }


  s.onNext("A")
  s.onNext("B")
  s.onNext("C")
  s.onNext("D")
}
```

Output is:  
Z  
ZA  
ZAB  
ZABC  
ZABCD  

You can see, first we created a stream (in this particular case, a Subject), which contains no element, but we prepare for future elements.
These future elements are coming by `onNext(e)`. The behavior is the same as was with the collection above, but in this case the stream is an infinite stream of elements. It ends only if `onComplete()` being called.

With these knowledge we can step further. Why don't we create complex type instead of a String.

### RxJava streams with model
So, let's create the same stream app with some complex type and you will see there is nothing complex, but some fundamental parts.

```scala
object StreamModel extends App{

  case class MainModel(value: Int = 0)

  val s = Subject[Int]()

  s.scan(MainModel()){ case (model, item) =>
    model.copy(model.value + item)
  }.foreach { model =>
    println(model)
  }


  s.onNext(10)
  s.onNext(20)
  s.onNext(30)
  s.onNext(40)
}
```
In this case, the output is:
MainModel(0)  
MainModel(10)  
MainModel(30)  
MainModel(60)  
MainModel(100)  

Which is nothing extra comparing to the previous example. But it highlights something with the `model.copy(model.value + item)` line.
There is something with it. I guess you will see, if I split it up:

```scala
case class MainModel(value: Int = 0)

class AppStream {
  val s = Subject[Int]()

  s.scan(MainModel()){ case (model, item) =>
    model.copy(model.value + item)
  }.foreach { model =>
    println(model)
  }

}

object StreamModel extends App{
  val app = new AppStream

  app.s.onNext(10)
  app.s.onNext(20)
  app.s.onNext(30)
  app.s.onNext(40)
}
```

So in this case we tried to separate concerns but we couldn't really do. This is because that line, I mentioned above is not belonging to the place where it is.
It should be moved to somewhere else. For example into the model. Try again:

```scala
case class MainModel(value: Int = 0) {
  def copy(newValue: Int): MainModel = MainModel(value + newValue)
}

class AppStream[T <: {def copy(newValue : Int) : T}](initialValue: T) {
  val s = Subject[Int]()

  s.scan(initialValue) { case (model, item) =>
    model.copy(item)
  }.foreach { model =>
    println(model)
  }

}

object StreamModel extends App {
  val app = new AppStream(MainModel())

  app.s.onNext(10)
  app.s.onNext(20)
  app.s.onNext(30)
  app.s.onNext(40)
}
```

Much better. We could generalize AppStream. Now we can create any kind of model, which is responsible for it's own job, to adding number to its contained value.
If you take a step back, you can acquire something. There is an application, which state was stored in the `MainModel` class.
And the application steps its state by actions, which in this cases are Ints (passing to the `onNext(e)` function).

But!
There are some problems with it:
- Application state is usually much more complex.
- Int is not a real representation for the possible actions in a real world application.

So deal with it!

### More complex state
Consider that, we have a little bit more complex application state. For example:

```scala
case class LeftModel(value: Int = 0) {
  def copy(newValue: Int): LeftModel = LeftModel(value + newValue)
}

case class RightModel(value: Int = 0) {
  def copy(newValue: Int): RightModel = RightModel(value + newValue)
}

case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  ...
}
```
What do you think, how should MainModel.copy look like?
Yes, it should delegate the call the underlying members' copy function:
```scala
case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  def copy(newValue: Int): MainModel = MainModel(left = left.copy(newValue), right = right.copy(newValue))
}
```
The rest of the code is the same as above. Because we generalized it already.
In this case the output is:  
MainModel(LeftModel(0),RightModel(0))  
MainModel(LeftModel(10),RightModel(10))  
MainModel(LeftModel(30),RightModel(30))  
MainModel(LeftModel(60),RightModel(60))  
MainModel(LeftModel(100),RightModel(100))

Great we dealt with the first problem. Let's jump to the second one. Try to represent every action which can happen in the application

### Actions

First we define a trait:
```scala
trait Action
```

Now we can define that the action can be anything in the application like: a button was clicked or the response from a server was arrived.
```scala
case class ButtonClicked(whichButton: SomeUid) extends Action
case class ResponseArrived(respone: SomeResponse) extends Action
```

In our case let's define two actions which represents two kind of value receive. Imagine two buttons with the corresponding text editor.
If you click the first button, the system adds the text editor value to the application state, and the other button does the same with substraction operation.
```scala
case class AddValueEmitted(value: Int) extends Action
case class SubValueEmitted(value: Int) extends Action
```

So here is two actions, which the system should be able to react on. The requirement is that, if `AddValueEmitted(value)` action was received,
the corresponding value of the LeftModel and the RightModel should be increased by the `value`. And the same behavior is expected for the 
`SubValueEmitted(value)` except that, it should decrease the value of both side of the state.

First we need to modify the subject, because it is no longer accepts Ints, and we need to modify the copy method's signature also. 
And in this step let's rename it to `step(action)`
```scala
class AppStream[T <: {def step(action : Action) : T}](initialValue: T) {
  val s = Subject[Action]()

  s.scan(initialValue) { case (model, item) =>
    model.step(item)
  }.foreach { model =>
    println(model)
  }
}
```

Now the app doesn't compile. It's normal, we need to modify the Models. As we saw above, the new `step(action)` function signature should be the following:
```scala
case class LeftModel(value: Int = 0) {
  def step(action: Action): LeftModel = ???
}
case class RightModel(value: Int = 0) {
  def step(action: Action): RightModel = ???
}
case class MainModel(left: LeftModel = LeftModel(), right: RightModel = RightModel()) {
  def step(action: Action): MainModel = MainModel(left = left.step(action), right = right.step(action))
}
```

We still left them unimplemented. How should the `step(action)` function work. It should somehow work with both of the action type: `AddValueEmitted(number)` and `SubValueEmitted(number)`.
Pattern matching is the right way to do this:
```scala
case class LeftModel(value: Int = 0) {
  def step(action: Action): LeftModel = action match {
    case AddValueEmitted(number) => LeftModel(value + number)
    case SubValueEmitted(number) => LeftModel(value + number)
  } 
}

case class RightModel(value: Int = 0) {
  def step(action: Action): RightModel = action match {
    case AddValueEmitted(number) => RightModel(value + number)
    case SubValueEmitted(number) => RightModel(value + number)
  }
}
```
In this example, both model reacts to both action type in the same way. They create a new instance of them, with the new value.  
Now the application compiles with this:
```scala
object StreamModelAction extends App {
  val app = new AppStream(MainModel())

  app.s.onNext(AddValueEmitted(10))
  app.s.onNext(AddValueEmitted(20))
  app.s.onNext(AddValueEmitted(30))
  app.s.onNext(AddValueEmitted(40))
}
```
And the output:  
MainModel(LeftModel(0),RightModel(0))  
MainModel(LeftModel(10),RightModel(10))  
MainModel(LeftModel(30),RightModel(30))  
MainModel(LeftModel(60),RightModel(60))  
MainModel(LeftModel(100),RightModel(100))  

### Conclusion
I think, with this basics, we can start to build our UI framework, which works with immutable persistent data structure as application state.
