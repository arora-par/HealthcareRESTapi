import java.time.Clock

import com.eclipsesource.schema.SchemaValidator
import com.google.inject.{AbstractModule, Provides}
import com.redis.RedisClient
import services.{ApplicationTimer, AtomicCounter, Counter}
/**
 * This class is a Guice module that tells Guice how to bind several
 * different types. This Guice module is created when the Play
 * application starts.

 * Play will automatically use any class called `Module` that is in
 * the root package. You can create modules in other locations by
 * adding `play.modules.enabled` settings to the `application.conf`
 * configuration file.
 */
class Module extends AbstractModule {

  override def configure() = {
    // Use the system clock as the default implementation of Clock
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)
    // Ask Guice to create an instance of ApplicationTimer when the
    // application starts.
    bind(classOf[ApplicationTimer]).asEagerSingleton()
    // Set AtomicCounter as the implementation for Counter.
    bind(classOf[Counter]).to(classOf[AtomicCounter])
  }

  @Provides
  def provideRedisClient(): RedisClient = {
    //val redisHost:String = play.Play.application.configuration.getString("redisHost")
    //val redisPort:Int = Play.configuration.getInt("redisPort").get
    val redis = new RedisClient("localhost", 6379)
    return redis;
  }

  @Provides
  def provideSchemaValidator(): SchemaValidator = {
    val validator = new SchemaValidator()
    return validator;
  }

}
