package server

import spray.json.DefaultJsonProtocol

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val personFormat = jsonFormat9(Person)
  implicit val friendFormat = jsonFormat1(Friends)
  //implicit val returnedEncPostsFormat = jsonFormat1(returnPosts)
  implicit val findPostsToReturnFormat = jsonFormat3(findPostsToReturn)
  implicit val postFormat = jsonFormat1(Post)
  implicit val pageFormat = jsonFormat5(Page)
}

case class Person(UserID:Int, Name: String, DOB: Int, Sex: String, Location: String, Occupation: String, Friends: Set[Int], SubscribedPages: Set[Int], Posts: Seq[String])
case class Friends(Friends: Seq[String])
//case class returnPosts(Seq[findPostsToReturn])
case class findPostsToReturn(var msg: String,var iv: String,var key: String)
case class Post (Post: String)
case class Page (OwnerID: Int, PageID: Int, Description: String, No_Of_Subscribers: Int, Posts: Seq[String])