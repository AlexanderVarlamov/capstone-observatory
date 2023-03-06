package observatory

import com.sksamuel.scrimage.implicits.*
import scala.util.Properties.isWin
import org.apache.log4j.{Level, Logger}

object Main extends App:

  if (isWin) System.setProperty("hadoop.home.dir", System.getProperty("user.dir") + "\\winutils\\hadoop-3.3.1")
  
end Main

