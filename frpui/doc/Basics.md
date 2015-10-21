# Functional Reactive UI framework


```scala
object Basics extends App {
  val list = List("A", "B", "C", "D")

  val result = list.scanLeft("Z") { (accu, item) =>
    accu + item
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


Rx Stream
```
object Stream extends App{
  val s = Subject[String]()

  s.scan("Z"){ (accu, item) =>
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



