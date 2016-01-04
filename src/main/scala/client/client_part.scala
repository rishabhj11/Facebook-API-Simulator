package client

import akka.actor._
import spray.http.BasicHttpCredentials
import spray.client.pipelining._
import scala.collection.mutable
import akka.actor.Cancellable
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import scala.util.{Success, Random}
import spray.json._
import spray.http.HttpRequest
import java.io.RandomAccessFile
import akka.actor.Props
import spray.http.HttpResponse
import java.net.URLEncoder
import java.security.SecureRandom
import javax.crypto.{SecretKey, Cipher, KeyGenerator}
import java.security._
import java.util.Base64
import javax.crypto.spec.{SecretKeySpec, IvParameterSpec}
import scala.collection.mutable.{SynchronizedMap, ListBuffer}

case class create_client()
case class stop()
case class friend_add(count:Int)
case class subscribe(page_count:Int)
case class post_msg_enc(var userID:Int,var Msg:String,var IV:String,var encrypted_keys:Seq[String])
case class post_to_friend_enc(var userID:Int,var friendID:Int,var Msg:String,var IV:String,var encrypted_keys:Seq[String])
case class wallPosts(var msg:String,var iv:String,var key:String)
//case class create_public_key()
case class getWallPosts(userId:Int,friendId:Int)
case class post_to_friend(userId:Int,friendId:Int,aesEncMessage:String,iv:String,pubKeyEncAesList:ListBuffer[String])

object postMsgJsonProtocol extends DefaultJsonProtocol {
  implicit val postMsgJsonFormat=jsonFormat4(post_msg_enc)
}

object postToFriendJsonProtocol extends DefaultJsonProtocol {
  implicit val postToFriendJsonFormat=jsonFormat5(post_to_friend_enc)
}

object getWallPostsJsonProtocol extends DefaultJsonProtocol {
  implicit val wallPostsGetJsonFormat=jsonFormat3(wallPosts)
}

object Client extends App {

  implicit val system = ActorSystem()

  import system.dispatcher

  var pubKeyMap = new mutable.HashMap[Int, PublicKey]() with mutable.SynchronizedMap[Int, PublicKey]


  val pipeline = sendReceive
  val SERVER = "http://localhost:8080/"
  val user_count = 1000
  for (i <- 1 to user_count) {
    val client_actor = system.actorOf(Props(new User(i, user_count)), "name" + i)
  }
  for (i <- 1 to user_count) {
    Thread.sleep(10)
    var client_actor = system.actorSelection("/user/name" + i)
    //client_actor ! create_public_key()
    client_actor ! create_client()
  }
  //Thread.sleep(10000l)

  /*----- adding friends to existing users ----------------------- */
  var friendStat1: Int = (0.37 * user_count).toInt
  for (i <- 1 to friendStat1) {
    var r: Random = new Random()
    var low: Int = 100
    var high: Int = 500
    var noOfFriends: Int = r.nextInt(high - low) + low
    var client_actor = system.actorSelection("/user/name" + i)
    client_actor ! friend_add(noOfFriends)
  }
  for (i <- (friendStat1 + 1) to user_count) {
    var noOfFriends = scala.util.Random.nextInt(100)
    while (noOfFriends == 0) {
      noOfFriends = scala.util.Random.nextInt(100)
    }
    var client_actor = system.actorSelection("/user/name" + i)
    client_actor ! friend_add(noOfFriends)
  }

  //---------------------Timer Start------------------
  /*-----Making a call to create pages-----------------------------------*/
  for (i <- 1 to 250) {
    //Set value 250
    var description = ""
    var id = scala.util.Random.nextInt(user_count)
    while (id == 0) {
      id = scala.util.Random.nextInt(user_count)
    }
    for (i <- 1 to 6) {
      description += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
    }
    Thread.sleep(10)
    val reply = pipeline(Post(SERVER + "create_page?" + "ownerID=" + id + "&pageID=" + i + "&description=" + description))
    reply.foreach {
      response =>
        //println(response.entity.asString)
    }
  }

  /*for (i <- 1 to user_count) {
    var actor = system.actorSelection("/user/name" + i)
    actor ! Start
  }*/

  /*----- Subscribe to pages ---------------------------------------*/
  for (i <- 1 to user_count) {
    var r1: Random = new Random()
    var low1: Int = 30
    var high1: Int = 50
    var page_count: Int = r1.nextInt(high1 - low1) + low1
    var client_actor = system.actorSelection("/user/name" + i)
    Thread.sleep(10)
    client_actor ! subscribe(page_count)
  }

  /*Thread.sleep(60000L)
  for (i <- 1 to user_count) {
    var actor = system.actorSelection("/user/name" + i)
    actor ! stop
  }

  pipeline(Get("http://localhost:8080/stop"))*/

  class User(id: Int, N: Int) extends Actor {

    import postMsgJsonProtocol._
    import postToFriendJsonProtocol._
    import getWallPostsJsonProtocol._

    var userID: Int = 0
    userID = id
    var name: String = ""
    var DOB: Int = 0
    var sex: String = ""
    var location: String = ""
    var occupation: String = ""
    var friendSet = Set.empty[Int]
    //var pvtKey: PrivateKey
    //var pubKey: PublicKey
    var statusUpdate: Cancellable = null
    var friendPost: Cancellable = null
    var postOnPage: Cancellable = null
    var fetchWall:Cancellable = null
    var friendsPerPost = 10 
    var postcount:Int=0

    //if(id==10){
      fetchWall = context.system.scheduler.schedule(Duration.create(10000, TimeUnit.MILLISECONDS),Duration.create(5000, TimeUnit.MILLISECONDS))(getPostsForUser)
    //}


    /*I-V generation*/
    var sRNG = SecureRandom.getInstance("SHA1PRNG")
    sRNG.setSeed(sRNG.generateSeed(32))
    var initVector = new Array[Byte](16)
    sRNG.nextBytes(initVector)

    var keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(512)
    var keypair = keyGen.genKeyPair
    var pvtKey = keypair.getPrivate
    var pubKey = keypair.getPublic
    pubKeyMap.put(userID, pubKey)


    //Scheduled function to post status update for each user at regular intervals
    def postStatusUpdate(): Unit = {
      var post: String = "" //Post would be a randonm generated sentence
      for (i <- 1 to 5) {
        post += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
      }
      val reply = pipeline(Post(SERVER + "add_post?" + "userID=" + userID + "&post=" + post))
      reply.foreach {
        response =>
          println(response.entity.asString)
      }
    }

    def postOnFriendsWall(): Unit = {
      var post: String = "" //Post would be a random generated sentence
      for (i <- 1 to 5) {
        post += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
      }
      var value = scala.util.Random.nextInt(friendSet.size)
      while (value == 0) {
        value = scala.util.Random.nextInt(friendSet.size)
      }
      var friend = friendSet.toVector(value)
      val reply = pipeline(Post(SERVER + "post_to_friend?" + "userID=" + userID + "&friend=" + friend + "&post=" + post))
      reply.foreach {
        response =>
          println(response.entity.asString)
      }
    }

    def postOnSubscribedPage(): Unit = {
      var post: String = "" //Post would be a random generated sentence
      for (i <- 1 to 5) {
        post += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
      }
      var pageID = scala.util.Random.nextInt(250)
      while (pageID == 0) {
        pageID = scala.util.Random.nextInt(250)
      }
      val reply = pipeline(Post(SERVER + "post_on_page?" + "userID=" + userID + "&pageID=" + pageID + "&post=" + post))
      reply.foreach {
        response =>
          //println(response.entity.asString)
      }
    }

    def getPostsForUser(){
      if(postcount>0){        
        self ! getWallPosts(userID,userID)
      }
    }

    def postOnOwnWall() {
      var postText: String = "" //Post would be a random generated sentence
      for (i <- 1 to 5) {
        postText += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
      }
      var cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")

      var aesKeyGen = KeyGenerator.getInstance("AES")
      aesKeyGen.init(128)
      var aesKey = aesKeyGen.generateKey()

      cipher.init(Cipher.ENCRYPT_MODE, aesKey, new IvParameterSpec(initVector))
      var aesKeyinString = Base64.getEncoder().encodeToString(aesKey.getEncoded)
      var encryptedMessage = cipher.doFinal(postText.getBytes("UTF-8"))
      var encryptedMessageString = Base64.getEncoder.encodeToString(encryptedMessage)
      var initVectorString = Base64.getEncoder.encodeToString(cipher.getIV)      
      if (friendsPerPost == 0) {
        friendsPerPost = 1
      }
      var aesListPublicEnc = new ListBuffer[String]()
      /*Inserting AES key encrypted with own public key*/
      var encWithOwnPubKeyInst = Cipher.getInstance("RSA/ECB/PKCS1Padding")
      encWithOwnPubKeyInst.init(Cipher.ENCRYPT_MODE, pubKey)
      var encWithOwnPubKey = encWithOwnPubKeyInst.doFinal(aesKeyinString.getBytes("UTF-8"))
      var encWithOwnPubKeyString = Base64.getEncoder.encodeToString(encWithOwnPubKey)
      var own_encryptedAESwithPub = userID + "!" + encWithOwnPubKeyString
      aesListPublicEnc += own_encryptedAESwithPub
      var accessToFriends = new ListBuffer[Int]()
      if (friendSet.size > 0) {
        for (i <- 1 to friendsPerPost) {
          var randomFriends_aes = friendSet.toVector(Random.nextInt(friendSet.size))
          var count: Int = 0
          while (!pubKeyMap.isDefinedAt(randomFriends_aes) || (accessToFriends.length > 0 && accessToFriends.contains(randomFriends_aes))) {
            count = count + 1
            randomFriends_aes = friendSet.toVector(Random.nextInt(friendSet.size))            
          }
         
          if (!accessToFriends.contains(randomFriends_aes)) {

            accessToFriends += randomFriends_aes

            /*Inserting AES key encrypted with friend's public key*/
            var randomFriendPubKey = pubKeyMap(randomFriends_aes)
            var pubKeyCipher_AES = Cipher.getInstance("RSA/ECB/PKCS1Padding")
            pubKeyCipher_AES.init(Cipher.ENCRYPT_MODE, randomFriendPubKey)
            var encryptedAesPublicKey = pubKeyCipher_AES.doFinal(aesKeyinString.getBytes("UTF-8"))
            var encryptedAesPublicKeyString = Base64.getEncoder.encodeToString(encryptedAesPublicKey)
            var encryptedAESwithPub_F = randomFriends_aes + "!" + encryptedAesPublicKeyString
            aesListPublicEnc += encryptedAESwithPub_F
          }
        }
        var request = URLEncoder.encode(post_msg_enc(userID, encryptedMessageString, initVectorString, aesListPublicEnc).toJson + "", "UTF-8")
        val reply = pipeline(Post(SERVER + "post_msg?" + "post_msg_json=" + request))
        reply.foreach {
          response =>
          //println(response.entity.asString)
        }
      }
    }
    // Posting Encrypted Posts on Frrinds Wall -----------------------------------------------
    def postOnFriendsPage() {
      if (!friendSet.isEmpty) {
        var postText: String = "" //Post would be a random generated sentence
        for (i <- 1 to 5) {
          postText += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
        }
        var cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")

        var aesKeyGen = KeyGenerator.getInstance("AES")
        aesKeyGen.init(128)
        var aesKey = aesKeyGen.generateKey()
        cipher.init(Cipher.ENCRYPT_MODE, aesKey, new IvParameterSpec(initVector))

        var aesKeyinString = Base64.getEncoder().encodeToString(aesKey.getEncoded)
        var encryptedMessage = cipher.doFinal(postText.getBytes("UTF-8"))
        var encryptedMessageString = Base64.getEncoder.encodeToString(encryptedMessage)
        var initVectorString = Base64.getEncoder.encodeToString(cipher.getIV)

        var aesListPublicEnc = new ListBuffer[String]()
        /*Inserting own public key-encrypted aes key*/
        var pubKeyCipher_AES = Cipher.getInstance("RSA/ECB/PKCS1Padding")
        pubKeyCipher_AES.init(Cipher.ENCRYPT_MODE, pubKey)
        var encryptedAesPublicKey = pubKeyCipher_AES.doFinal(aesKeyinString.getBytes("UTF-8"))
        var encryptedAesPublicKeyString = Base64.getEncoder.encodeToString(encryptedAesPublicKey)
        var encryptedAESwithPub = userID + "!" + encryptedAesPublicKeyString
        aesListPublicEnc += encryptedAESwithPub

        var random_friend = friendSet.toVector(Random.nextInt(friendSet.size))
        while (!pubKeyMap.isDefinedAt(random_friend)) {
          random_friend = friendSet.toVector(Random.nextInt(friendSet.size))
        }

        var random_friend_pubKey = pubKeyMap(random_friend)
        pubKeyCipher_AES = Cipher.getInstance("RSA/ECB/PKCS1Padding")
        pubKeyCipher_AES.init(Cipher.ENCRYPT_MODE, random_friend_pubKey)
        encryptedAesPublicKey = pubKeyCipher_AES.doFinal(aesKeyinString.getBytes("UTF-8"))
        encryptedAesPublicKeyString = Base64.getEncoder.encodeToString(encryptedAesPublicKey)
        encryptedAESwithPub = random_friend + "!" + encryptedAesPublicKeyString
        aesListPublicEnc += encryptedAESwithPub

        self ! post_to_friend(userID,random_friend,encryptedMessageString,initVectorString,aesListPublicEnc)


/*        var request = URLEncoder.encode(post_to_friend_enc(userID, random_friend, encryptedMessageString, initVectorString, aesListPublicEnc).toJson + "", "UTF-8")
        var response = pipeline(Post(SERVER + "post_to_friend?" + "post_to_friend_json=" + request))
        response.foreach {
          response =>
          //println(response.entity.asString)
        }*/
      }
    }


    def receive = {
      case create_client() => {
        for (i <- 1 to 2) {
          name += (scala.util.Random.alphanumeric.take(5).mkString.toLowerCase + "+")
        }
        DOB = scala.util.Random.nextInt(100000)
        val gender = Array("M", "F")
        sex = gender(scala.util.Random.nextInt(2))
        location = scala.util.Random.alphanumeric.take(6).mkString.toLowerCase
        occupation = scala.util.Random.alphanumeric.take(6).mkString.toLowerCase
        val reply = pipeline(Post(SERVER + "create?" + "userID=" + userID + "&name=" + name + "&DOB=" + DOB + "&sex=" + sex + "&location=" + location + "&occupation=" + occupation))
        /*reply.foreach {
          response =>
            println(response.entity.asString)
        }*/
        //self ! create_public_key()
        //Thread.sleep(100)

        /*friendPost = context.system.scheduler.schedule(Duration.create(50, TimeUnit.MILLISECONDS),
          Duration.create(2100, TimeUnit.MILLISECONDS))(postOnOwnWall)*/
      }

      case friend_add(count: Int) => {
        for (i <- 1 to count) {
          //Thread.sleep(10)
          var rand = scala.util.Random.nextInt(user_count)
          while (rand == 0 || rand == userID || friendSet.contains(rand)) {
            rand = scala.util.Random.nextInt(user_count)
          }
          friendSet += rand
          //Thread.sleep(10)
          val reply = pipeline(Post(SERVER + "add_friend?" + "userID=" + userID + "&friend=" + rand))
          /*reply.foreach {
            response =>
              println(response.entity.asString)
          }*/
        }
        statusUpdate = context.system.scheduler.schedule(Duration.create(10, TimeUnit.MILLISECONDS),
          Duration.create(4000, TimeUnit.MILLISECONDS))(postOnFriendsPage)
      }

      //Public-Private Key generation
      /*case create_public_key() => {
        var keyGen = KeyPairGenerator.getInstance("RSA")
        keyGen.initialize(1024)
        var keypair = keyGen.genKeyPair
        var pvtKey = keypair.getPrivate
        var pubKey = keypair.getPublic
        pubKeyMap.put(userID, pubKey)
      }*/

      case post_to_friend(userId:Int,friendId:Int,aesEncMessage:String,iv:String,pubKeyEncAesList:ListBuffer[String]) => {
        var request = URLEncoder.encode(post_to_friend_enc(userId,friendId,aesEncMessage,iv,pubKeyEncAesList).toJson+"","UTF-8")
        var response=pipeline(Post(SERVER+ "post_to_friend?"+"post_to_friend_json="+request))
        //response.foreach{value => println(value.entity.asString)}
        postcount += 1
      }

      case subscribe(page_count: Int) => {
        for (i <- 1 to page_count) {
          //Thread.sleep(10)
          var rand = scala.util.Random.nextInt(250)
          while (rand == 0) {
            rand = scala.util.Random.nextInt(250)
          }
          val reply = pipeline(Post(SERVER + "subscribe?" + "userID=" + userID + "&pageID=" + rand))
          /*reply.foreach {
            response =>
              println(response.entity.asString)
          }*/
        }
        postOnPage = context.system.scheduler.schedule(Duration.create(10, TimeUnit.MILLISECONDS),
          Duration.create(7000, TimeUnit.MILLISECONDS))(postOnSubscribedPage)
      }

      case getWallPosts(userId:Int,friendId:Int) => {        
        import getWallPostsJsonProtocol._
        var wallPostEncrypt_RSA = Cipher.getInstance("RSA/ECB/PKCS1Padding")
        var wallPostEncrypt_AES = Cipher.getInstance("AES/CBC/PKCS5Padding")
        var response = pipeline(Get(SERVER + "get_wall_posts?"+ "userid="+ userID + "&friendid=" + friendId))
        //response.foreach{value => println(value.entity.asString)}
        response onComplete {
          case Success(result) => {
            var postsList = result.entity.data.asString.parseJson.convertTo[List[wallPosts]]            
            if(postsList.length>0){
              for (wallPost <- postsList){
                //println(wallPost)
                //println("pvtKey :"+Base64.getEncoder.encodeToString(pvtKey.getEncoded))
                wallPostEncrypt_RSA.init(Cipher.DECRYPT_MODE, pvtKey)
                var aeskey=wallPost.key.map(c => if(c == ' ') '+' else c)
                var iv=wallPost.iv.map(c => if(c == ' ') '+' else c)
                var msg=wallPost.msg.map(c => if(c == ' ') '+' else c)
                var aesKeyBytes = Base64.getDecoder.decode(new String(wallPostEncrypt_RSA.doFinal(Base64.getDecoder.decode(aeskey))))
                var origAesKey:SecretKey = new SecretKeySpec(aesKeyBytes,0,aesKeyBytes.length,"AES")                
                wallPostEncrypt_AES.init(Cipher.DECRYPT_MODE, origAesKey, new IvParameterSpec(Base64.getDecoder.decode(iv)))
                println("Retrieving user:"+userId+"'s Post")
                println("Encrypted Post : "+msg)
                println("Decrypted Post : "+new String(wallPostEncrypt_AES.doFinal(Base64.getDecoder.decode(msg))).map(c => if(c == '+') ' ' else c))
              }
            }
          }
        }
      }
    }
  }
}




