package exercises

object OOPExtended extends App{

  val a = new MyLinkedList[Int](1,new MyLinkedList[Int](2,EmptyList));
  val b = new MyLinkedList[String]("A",new MyLinkedList[String]("B",EmptyList));
//  println(a.toString);
//  val b = a.filter(x => x==1);
//  println(b.toString)
//  val c = a.map(x => x*2);
//  println(c.toString)
//  val d = a.flatMap(new MyTransformer[Int,MyList[Int]] {
//     def transform(x: Int) = new MyLinkedList[Int](x,new MyLinkedList[Int](x+1,EmptyList))
//  })
//  println(d.toString)

  val forCompre = for {
    x <- a
    y <- b
  } yield x+"-"+y
  println(forCompre)
  println(a.sort((x,y) => y-x).toString)
}

abstract class MyList[+A] {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](i: B): MyList[B]

  def printElements : String
  override def toString: String = "["+printElements+"]"

  def map[B]: MyTransformer[A,B] => MyList[B]

  def filter: MyPredicate[A] => MyList[A]

  def ++[B >: A]: MyList[B] => MyList[B]

  def flatMap[B]: MyTransformer[A,MyList[B]] => MyList[B]

  def forEach(f : A => Unit): Unit

  def sort(f: (A,A) => Int) : MyList[A]
}

object EmptyList extends MyList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](i: B): MyList[B] = new MyLinkedList(i, EmptyList)

  override def printElements: String = ""

  override def map[B]: MyTransformer[Nothing,B] => MyList[B] = (myTransformer) => EmptyList

  override def filter: MyPredicate[Nothing] => MyList[Nothing] = (myPredicate) => EmptyList

  override def flatMap[B] : MyTransformer[Nothing, MyList[B]] => MyList[B] = (myTransformer) => EmptyList

  override def ++[B >: Nothing]: MyList[B] => MyList[B] = list => list

  override def forEach(f: Nothing => Unit): Unit = ()

  override def sort(f: (Nothing, Nothing) => Int): MyList[Nothing] = EmptyList
}

class MyLinkedList[+A](h: A, t: MyList[A]) extends MyList[A] {

  override def head: A = h

  override def add[B >: A](i: B): MyList[B] = new MyLinkedList(i, this)

  override def isEmpty: Boolean = false

  override def tail: MyList[A] = t

  override def printElements: String = {
    if(t.isEmpty) h+""
    else h+" "+t.printElements
  }

  override def map[B] : MyTransformer[A, B] => MyList[B] = {(myTransformer)=>
    new MyLinkedList[B](myTransformer.transform(h),t.map(myTransformer))
  }

  override def filter: MyPredicate[A] => MyList[A] = { (myPrediction) =>
    if(myPrediction.test(h))
      new MyLinkedList[A](h,t.filter(myPrediction))
    else
      t.filter(myPrediction)
  }

  override def ++[B >: A] : MyList[B] => MyList[B] = (list) => new MyLinkedList[B](h,t++list)

  override def flatMap[B] : MyTransformer[A, MyList[B]] => MyList[B] = (myTransformer) =>
    myTransformer.transform(h) ++ t.flatMap(myTransformer)

  override def forEach(f: A => Unit): Unit = {
    f(h)
    t.forEach(f)
  }

  override def sort(f: (A, A) => Int): MyList[A] = {
    def insert(a: A, value: MyList[A]): MyList[A] =
      if(value.isEmpty) new MyLinkedList(a,EmptyList)
      else if(f(a,value.head) <= 0) new MyLinkedList(a,value)
      else new MyLinkedList(value.head,insert(a,value.tail))
    val sortedTail = t.sort(f)
    insert(h,sortedTail)
  }
}

trait MyPredicate[-T]{
  def test(elem: T) : Boolean
}

trait MyTransformer[-A,B]{
  def transform(a : A): B
}

class EvenPredicate[T] extends MyPredicate[Int]{
  override def test(t: Int): Boolean = t%2==0
}

