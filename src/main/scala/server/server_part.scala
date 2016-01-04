package server

import java.net.URLDecoder
import java.util.HashMap
import java.util.concurrent.TimeUnit
import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.routing.{RoundRobinPool, RoundRobinRouter}
import akka.util.Timeout
import scala.collection.mutable
import org.json4s.ShortTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import spray.routing.SimpleRoutingApp
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import spray.can.Http
import spray.can.server.Stats
import play.api.libs.json.Json
import spray.json._
import org.json4s.ShortTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import spray.json.DefaultJsonProtocol

/**
 * Created by rishabh on 11/19/15.
 */

case class create_user(userID: Int, name: String, dob: Int, sex: String, location: String, occupation: String)
case class display_profile(userID: Int)
case class addFriend (userID:Int, friend:Int)
case class showFriends (userID:Int)
case class statusUpdate (userID:Int, post:String)
case class getPost (userID:Int, friend:Int)
case class createPage (ownerID:Int,pageID:Int,description:String)
case class subscribeToPage (userID:Int,pageID:Int)
case class postOnPage (userID:Int,pageID:Int,post:String)
case class showPage (pageID:Int)
case class postByPage (userID:Int,pageID:Int,post:String)
case class postToFriend (userID:Int, friend:Int, post:String)
case class wall_post (json_object:String)
case class post_to_friend (json_object:String)
case class get_wall_posts (userID:Int, friendID:Int)
//case class findPostsToReturn(var msg: String, var iv: String, var key: String)

case class update_users()
case class update_friends()
case class update_posts()
case class update_pages()
case class show_stats()


object Server extends App with SimpleRoutingApp {

  implicit val System = ActorSystem()

  import System.dispatcher

  implicit val timeout = Timeout(1.seconds)

  val handler_actor = System.actorOf(Props[Handler])
  val stats_actor = System.actorOf(Props[Program_Stats])

  class user {
    var userID:Int = 0
    var name: String = ""
    var DOB: Int = 0
    var sex: String = ""
    var location: String = ""
    var occupation: String = ""
    var friendSet = Set.empty [Int]
    var postObjectList = List[postClass]()
    var subscribedPages = Set.empty [Int]
  }

  class page {
    var ownerID:Int = 0
    var pageID:Int = 0
    var description:String = ""
    var subscriberSet = Set.empty [Int]
    subscriberSet += ownerID
    var pagepostObjectList = List[String]()
  }

  class postClass(Msg:String, IV:String, encrypted_keys:ListBuffer[String]) {
    var msg: String = Msg
    var iv: String = IV
    var pub_aes_keys: ListBuffer[String]= encrypted_keys
  }

/*  class findPostsToReturn(Msg:String, IV:String) {
    var msg: String = Msg
    var iv: String = IV
    var key: String = ""
  }*/

  class postsToReturn(){
    var listofPosts = ListBuffer[findPostsToReturn]()
  }

  class Handler extends Actor {
    private implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[String])))
    var map_user = new mutable.HashMap[Int, user] () with mutable.SynchronizedMap[Int, user]
    var map_page = new mutable.HashMap[Int, page] () with mutable.SynchronizedMap[Int, page]

    def receive = {
      case create_user (userID: Int, name: String, dob: Int, sex: String, location: String, occupation: String) =>
        val obj = new user()
        obj.userID = userID
        obj.name = name
        obj.DOB = dob
        obj.sex = sex
        obj.location = location
        obj.occupation = occupation
        map_user.put(userID, obj)
        stats_actor ! update_users()
        sender ! ("Created user # "+ userID)

      case display_profile (userID: Int) =>
        if (map_user.isDefinedAt(userID)) {
          var user_obj = map_user(userID)
          //1sender ! writePretty(Person(user_obj.userID, user_obj.name, user_obj.DOB, user_obj.sex, user_obj.location, user_obj.occupation, user_obj.friendSet, user_obj.subscribedPages, user_obj.postObjectList))
        }
        else "Object not present"

      case addFriend (userID:Int, friend:Int) =>
        if ((map_user.isDefinedAt(userID)) && (map_user.isDefinedAt(friend))) {
          var user_obj1 = map_user(userID)
          var user_obj2 = map_user(friend)
          user_obj1.friendSet += friend
          map_user.put(userID, user_obj1)
          user_obj2.friendSet += userID
          map_user.put(friend, user_obj2)
          stats_actor ! update_friends()
          sender ! (user_obj1.name + " became friends with " + user_obj2.name)
        }
        else "Object not present"

      case showFriends (userID:Int) => {
        if (map_user.isDefinedAt(userID)) {
          var obj_user = map_user(userID)
          var tempList = List[String]()
          for (x <- obj_user.friendSet) {
            tempList = map_user(x).name :: tempList
          }
          sender ! writePretty(Friends(tempList))
        }
        else "Object not present"
      }

      case statusUpdate (userID:Int, post:String) => {
        if (map_user.isDefinedAt(userID)) {
          var user_obj = map_user(userID)
          //1user_obj.postObjectList = post :: user_obj.postObjectList
          map_user.put(userID, user_obj)
          stats_actor ! update_posts()
          sender ! (user_obj.name + " posted status update")
        }
        else "Object not present"
      }

      case getPost (userID:Int, friend:Int) =>
        if ((map_user.isDefinedAt(userID)) && (map_user.isDefinedAt(friend))) {
          var user_obj1 = map_user(userID)
          var user_obj2 = map_user(friend)
          var size = user_obj2.postObjectList.size
          //1sender ! writePretty(Post(user_obj2.postObjectList(size-1)))
        }
        else "Object not present"

      case createPage (ownerID:Int,pageID:Int,description:String) =>
        if (map_user.isDefinedAt(ownerID)) {
          val obj1 = new page()
          val obj2 = map_user(ownerID)
          obj2.subscribedPages += pageID
          obj1.ownerID = ownerID
          obj1.pageID = pageID
          obj1.description = description
          map_page.put(pageID, obj1)
          stats_actor ! update_pages()
          sender ! ("Created page with ID # " + pageID)
        }
        else "Object not present"

      case subscribeToPage (userID:Int,pageID:Int) =>
        if ((map_user.isDefinedAt(userID)) && (map_page.isDefinedAt(pageID))) {
          var obj_user = map_user(userID)
          var obj_page = map_page(pageID)
          obj_page.subscriberSet += userID
          obj_user.subscribedPages += pageID
          map_user.put(userID, obj_user)
          map_page.put(pageID, obj_page)
          sender ! (obj_user.name + " subscribed")
        }
        else ("Object not present")

      case postOnPage (userID:Int,pageID:Int,post:String) => {
        if ((map_user.isDefinedAt(userID)) && (map_page.isDefinedAt(pageID))) {
          var obj_page = map_page(pageID)
          var obj_user = map_user(userID)
          obj_page.pagepostObjectList = obj_user.name + " : " + post :: obj_page.pagepostObjectList
          map_page.put(pageID, obj_page)
          stats_actor ! update_posts()
          sender ! (obj_user.name + " posted on the page")
        }
        else "Object not present"
      }

      case postByPage (userID:Int,pageID:Int,post:String) =>
        if(map_page.isDefinedAt(pageID)) {
          var obj_page = map_page(pageID)
          if (obj_page.ownerID == userID) {     //Privacy Settings: Only the admin can create a post for the page
            obj_page.pagepostObjectList = "Page Post : " + post :: obj_page.pagepostObjectList
            map_page.put(pageID, obj_page)
            stats_actor ! update_posts()
            sender ! ("A new post has came up on page # " + pageID)
          }
          else {
            sender ! ("Cannot post. User is not an admin")
          }
        }
        else "Object not present"

      case postToFriend (userID:Int,friend:Int, post:String) =>
        if ((map_user.isDefinedAt(userID)) && (map_user.isDefinedAt(friend))) {
          var user_obj1 = map_user(userID)
          var user_obj2 = map_user(friend)
          if (user_obj2.friendSet.contains(userID)) {  //Privacy Settings- Can post only on friend's wall
            //1user_obj2.postObjectList = user_obj1.name + " : " + post :: user_obj2.postObjectList
            map_user.put(friend, user_obj2)
            stats_actor ! update_posts()
            sender ! (user_obj1.name + " posted on the wall of " + user_obj2.name)
          }
          else {
            sender ! (user_obj2.name + " is not a friend of " + user_obj1.name)
          }
        }
        else "Object not present"

      case showPage (pageID:Int) =>
        if(map_page.isDefinedAt(pageID)) {
          var obj_page = map_page(pageID)
          sender ! writePretty(Page(obj_page.ownerID, obj_page.pageID, obj_page.description, obj_page.subscriberSet.size, obj_page.pagepostObjectList))
        }
        else "Object not present"

      case wall_post (json_object:String) => {
        val json_parsed = Json.parse(json_object)
        var userID: Int = json_parsed.\("userID").as[Int]
        val encrypted_post = new postClass(json_parsed.\("Msg").as[String],json_parsed.\("IV").as[String],json_parsed.\("encrypted_keys").as[ListBuffer[String]] )
        if (map_user.isDefinedAt(userID)) {
          var user_obj = map_user(userID)
          user_obj.postObjectList = encrypted_post :: user_obj.postObjectList
          map_user.put(userID, user_obj)
          stats_actor ! update_posts()
          sender ! (user_obj.name + " posted status update")
        }
        else "Object not present"
      }

      case get_wall_posts(userID:Int, friendID:Int) => {
        if (userID == friendID) {
          if (map_user.isDefinedAt(userID)) {
            var user_obj1 = map_user(userID)
            var list_posts_to_return = new ListBuffer[findPostsToReturn]()
            //for (i <- 0 to (user_obj1.postObjectList.size-1)){
            for (postobject <- user_obj1.postObjectList){
              var count = 0
              var posts_to_return = new findPostsToReturn(postobject.msg, postobject.iv, "")
              var aes_keys_list = postobject.pub_aes_keys
              /*if(userID == 10){
                println("List " + aes_keys_list)
              }*/
              for (encAes_key <- aes_keys_list){ // foreach
                var k = encAes_key.indexOf("!")
                if (encAes_key.substring(0,(k)).toInt==userID ){
                  posts_to_return.key = encAes_key.substring(k+1,encAes_key.length)
                  count += 1
                }
              }
              if(count>0){
                list_posts_to_return += posts_to_return
              }
            }
            /*if(userID == 10){
              println("Length"+list_posts_to_return.length)
            }*/
            sender ! writePretty(list_posts_to_return)
          }
          else "Object not present"
        }
        else {
          if ((map_user.isDefinedAt(userID)) && (map_user.isDefinedAt(friendID))) {
            var user_obj1 = map_user(userID)
            var user_obj2 = map_user(friendID)
            if (user_obj2.friendSet.contains(userID)) { //Privacy Settings- Can view only friend's wall
            var list_posts_to_return = new ListBuffer[findPostsToReturn]()
              for (postobject <- user_obj1.postObjectList){
                var count = 0
                var posts_to_return = new findPostsToReturn(postobject.msg, postobject.iv, "")
                var aes_keys_list = postobject.pub_aes_keys
                for (encAes_key <- aes_keys_list){
                  var k = encAes_key.indexOf("!")
                  if (encAes_key.substring(0,(k)).toInt==userID){
                    posts_to_return.key = encAes_key.substring(k+1,encAes_key.length)
                    count += 1
                  }
                }
                if(count>0){
                  list_posts_to_return += posts_to_return
                }
              }
              sender ! writePretty(list_posts_to_return)
            }              
          }
          else "Object not present"
        }
      }

      case post_to_friend (json_object:String) => {
        val json_parsed = Json.parse(json_object)
        var userID: Int = json_parsed.\("userID").as[Int]
        var friendID: Int = json_parsed.\("friendID").as[Int]
        val encrypted_post = new postClass(json_parsed.\("Msg").as[String],json_parsed.\("IV").as[String],json_parsed.\("encrypted_keys").as[ListBuffer[String]] )
        if ((map_user.isDefinedAt(userID)) && (map_user.isDefinedAt(friendID))) {
          var user_obj1 = map_user(userID)
          var user_obj2 = map_user(friendID)
          if (user_obj2.friendSet.contains(userID)) {  //Privacy Settings- Can post only on friend's wall
            user_obj2.postObjectList = encrypted_post :: user_obj2.postObjectList
            map_user.put(friendID, user_obj2)
            stats_actor ! update_posts()
            sender ! (user_obj1.name + " posted on the wall of " + user_obj2.name)
          }
          else {
            sender ! (user_obj2.name + " is not a friend of " + user_obj1.name)
          }
        }
        else "Object not present"
      }

    }
  }

  /* Process to find the number of requests getting processed------------*/
  class Program_Stats extends Actor {
    var noOfUsers:Int = 0
    var noOfFriends:Int = 0
    var noOfPosts:Int = 0
    var noOfPages:Int = 0
    def receive = {
      case update_users() =>
      {
        noOfUsers += 1
      }
      case update_friends() => {
        noOfFriends += 1
      }
      case update_posts() => {
        noOfPosts += 1
      }
      case update_pages() => {
        noOfPages += 1
      }
      case show_stats() => {
        println("NumberOfUsers:" + noOfUsers + " NumberOfFriends:" + noOfFriends + " NumberOfPosts:" + noOfPosts + " NumberOfPages:" + noOfPages)
      }
    }
  }

  System.scheduler.schedule(100 milliseconds,5000 milliseconds){
    stats_actor ! show_stats()
    System.actorSelection("/user/IO-HTTP/listener-0") ? Http.GetStats onSuccess {
      case x: Stats => println("Uptime: " + x.uptime.toSeconds + " seconds" + " RequestsProcessed: " + x.totalRequests)// + " Efficiency: " + ((x.totalRequests)/(x.uptime.toSeconds)).toInt + " requests/sec")
    }
  }

  startServer(interface = "localhost", port = 8080) {
    post {
      path("create") {
        parameter("userID", "name", "DOB", "sex", "location", "occupation") { (userID, name, DOB, sex, location, occupation) =>

          complete {
            (handler_actor ? create_user(userID.toInt, name, DOB.toInt, sex, location, occupation))
              .mapTo[String]
              .map(s => s"$s" )
          }
        }
      }
    }~
      post {
        path("add_friend") {
          parameter("userID", "friend") { (userID, friend) =>
            complete {
              (handler_actor ? addFriend(userID.toInt, friend.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("add_post") {
          parameter("userID","post") { (userID, post) =>
            complete {
              (handler_actor ? statusUpdate(userID.toInt, post))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("create_page") {
          parameter("ownerID","pageID","description") { (ownerID,pageID,description) =>
            complete {
              (handler_actor ? createPage(ownerID.toInt,pageID.toInt,description))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("subscribe") {
          parameter("userID","pageID") { (userID,pageID) =>
            complete {
              (handler_actor ? subscribeToPage(userID.toInt,pageID.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("post_on_page") {
          parameter("userID","pageID","post") { (userID,pageID,post) =>
            complete {
              (handler_actor ? postOnPage(userID.toInt,pageID.toInt,post))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("post_by_page") {
          parameter("userID","pageID","post") { (userID,pageID,post) =>
            complete {
              (handler_actor ? postByPage(userID.toInt,pageID.toInt,post))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("post_to_friend") {
          parameter("userID","friend","post") { (userID,friend,post) =>
            complete {
              (handler_actor ? postToFriend(userID.toInt,friend.toInt,post))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      get {
        path("profile") {
          parameter("userID") { userID =>
            complete {
              (handler_actor ? display_profile(userID.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      get {
        path("friends") {
          parameter("userID") { (userID) =>
            complete {
              (handler_actor ? showFriends(userID.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      get {
        path("post") {
          parameter("userID","friend") { (userID,friend) =>
            complete {
              (handler_actor ? getPost(userID.toInt, friend.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      get {
        path("show_page") {
          parameter("pageID") { (pageID) =>
            complete {
              (handler_actor ? showPage(pageID.toInt))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("post_msg") {
          parameter("post_msg_json"){ json_object =>
            complete {
              (handler_actor ? wall_post(URLDecoder.decode(json_object,"UTF-8")))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
      post {
        path("post_to_friend") {
          parameter("post_to_friend_json"){ json_object =>
            complete {
              (handler_actor ? post_to_friend(URLDecoder.decode(json_object,"UTF-8")))
                .mapTo[String]
                .map(s => s"$s" )
            }
          }
        }
      }~
    get {
      path("get_wall_posts") {
        parameter("userid","friendid"){ (userID, friendID) =>
          complete {
            (handler_actor ? get_wall_posts(userID.toInt, friendID.toInt))
              .mapTo[String]
              .map(s => s"$s" )
          }
        }
      }
    }
  }
}
