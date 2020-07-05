package exercises

import java.util.NoSuchElementException

object SocialNetwork extends App {

  var persons = Map[String, List[String]]()

  def isPersonExists(name: String) = persons.contains(name)

  def isFriendExists(name: String,friend: String) = persons(name).contains(friend)


  def addPerson(name: String, friends: List[String] = List()): Unit = {
    if(!isPersonExists(name)) {
        println(s"Adding $name")
        persons = persons + (name -> friends)
        if (friends.size > 0) {
          friends.foreach(addPerson(_))
        }
        //println(persons)
      }
  }


  def removePerson(name: String) = {
    if(isPersonExists(name)) {
      println(s"Removing $name")
      persons = persons.view.filterKeys(!_.equals(name)).mapValues(x => x.filter(!_.equals(name))).toMap
    }
    //println(persons)
  }

  def addFriend(name: String, friend: String):Unit = {
    if(isPersonExists(name) && !isFriendExists(name,friend)) {
      println(s"Adding friend for $name")
      val friendList = persons(name) :+ friend
      persons = persons + (name -> friendList)
      addFriend(friend,name)
    }
   // println(persons)
  }

  def removeFriend(name: String, friend: String):Unit = {
    if(isPersonExists(name) && !isFriendExists(name,friend)) {
      println(s"Removing friend for $name")
      val friendList = persons(name).filter(!_.equals(friend))
      persons = persons + (name -> friendList)
      removeFriend(friend,name)
    }
   // println(persons)
  }

  def getFriendsCount(name: String) = {
    if(isPersonExists(name)) persons(name).size
    else 0
  }

  def getFamousPerson() = {
    println(persons.maxBy(_._2.size)._1)
  }

  def noFriendsPeopleCount() = {
    println(persons.count(pair => pair._2.isEmpty))
  }

  addPerson("Tony",List("Bruce","Natasha","Steve"))
  addPerson("Tony")
  addFriend("Tony", "Bruce")
  addFriend("Tony", "Steve")
  removeFriend("Tony", "Bruce")
  removePerson("Steve")
  getFamousPerson()
  noFriendsPeopleCount()

}
