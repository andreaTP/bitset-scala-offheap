package jmh

import org.openjdk.jmh.annotations._
import scala.offheap._

@State(Scope.Thread)
class GCBitmapSearch {
  @Param(scala.Array("20"))
  var n: Int = _

  @Benchmark
  def run = GCAlgo.run(n)
}

@State(Scope.Thread)
class OffheapBitmapSearch {
  @Param(scala.Array("20"))
  var n: Int = _

  @Benchmark
  def run = OffheapAlgo.run(n)
}

object algo {
  type BS = {
    def set(n: Int): Unit
    def set(n: Int, value: Boolean): Unit
    def get(n: Int): Boolean
  }

  def bitmapSearch(text: String, pattern: String, newBitSet: Int => BS): Int = {
    val m = pattern.length
    var result = -1
    if (m == 0) return 0
    else {
      val R = newBitSet(m+1)
      R.set(0)

      var i, k = 0
      val textLength = text.length()
      while(i < textLength && result == -1) {
        k = m
        while(k > 0) {
          R.set(k, (R.get(k - 1) && (text(i) == pattern(k-1))))
          k -= 1
        }
        if (R.get(m))
          result = i-m+1

        i += 1
      }
      return result
    }
    return 1
  }
}


object GCAlgo {
  def run(n: Int) = {
    var i = n
    while (i > 0) {

      algo.bitmapSearch(data.text, data.pattern, (m) => new java.util.BitSet(m+1))

      i -= 1
    }
  }
}

object OffheapAlgo {
  def run(n: Int) = {
    var i = n
    while (i > 0) {
      implicit val props = Region.Props(Pool())
      implicit val r = Region.open

      algo.bitmapSearch(data.text, data.pattern, (m) => new eu.unicredit.offheap.BitSet(m+1))

      r.close
      i -= 1
    }
  }
}

object data {
  val pattern = "Aliquam nec odio odio"
  val text = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent neque mauris, consequat et mauris in, varius blandit leo. Aliquam non ornare libero. Sed dictum vel felis vitae pretium. Phasellus eget erat eu sem egestas pretium vel at augue. Sed blandit ex hendrerit congue fermentum. Duis eu leo sit amet lorem mattis sagittis. Nam euismod vitae enim a porttitor. Sed vestibulum massa id imperdiet convallis. Duis viverra fermentum bibendum. Morbi vulputate ipsum id urna luctus, ac pharetra lorem lacinia. Etiam blandit lacinia nisl sed ultrices. Sed condimentum urna eu enim hendrerit commodo. In ut fermentum tellus. Suspendisse pulvinar posuere leo.

Morbi a mauris in urna vehicula commodo. Etiam suscipit rutrum imperdiet. Vivamus consequat iaculis velit a dapibus. Cras ultricies hendrerit mollis. Nulla vel fringilla mauris. Morbi sed pretium enim. Fusce quis turpis et ipsum euismod auctor. Suspendisse imperdiet nisl sit amet vulputate fringilla. Maecenas id finibus nulla. Suspendisse suscipit egestas nunc ut tempus. Suspendisse ultrices, ipsum eu mollis pretium, enim elit faucibus massa, non tempus tortor libero sit amet erat. Pellentesque justo ex, mattis nec pellentesque in, efficitur nec dui. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aliquam molestie velit vitae mi suscipit placerat. Ut lacinia mi eget lacus mollis, sit amet convallis tellus malesuada.

Morbi nec ullamcorper elit, sit amet blandit augue. Quisque ultricies, sapien ac ultricies convallis, est ante laoreet lectus, vitae finibus neque nisi sit amet enim. Duis non nunc auctor dolor consectetur feugiat. Aenean consectetur justo justo, quis feugiat sem fringilla eu. Integer ornare ante sit amet libero viverra, quis hendrerit leo molestie. Donec sit amet ex mi. Proin facilisis nulla vitae sodales aliquet.

Maecenas ornare nisi a rutrum consectetur. Pellentesque convallis tellus non imperdiet dictum. Praesent malesuada eu orci id finibus. Vivamus ac odio rhoncus, laoreet orci rhoncus, vehicula purus. Vivamus ac velit laoreet, auctor ipsum vitae, varius velit. Sed in venenatis felis. Curabitur eu consectetur leo. Sed fringilla eu arcu at gravida. Proin rutrum elementum imperdiet. Etiam in mi eget leo consectetur venenatis vitae at odio.

Curabitur vehicula, justo vitae faucibus maximus, enim urna mollis velit, at porta neque tortor vitae libero. Morbi vitae pulvinar lectus. Sed ultricies ligula sed nibh ultrices commodo. Nunc at sapien ut diam cursus dictum in quis nunc. Nullam sed quam tempus, placerat elit sit amet, viverra lorem. Pellentesque porttitor in ante eu pulvinar. Nulla id scelerisque orci. Ut at pharetra tortor. Curabitur condimentum commodo imperdiet. Nunc semper fringilla urna ut placerat. Nulla vitae dui dolor. Aliquam erat volutpat. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Curabitur rhoncus ligula augue, eget suscipit massa volutpat ut. Suspendisse fringilla turpis eget dui consequat, ut sollicitudin lectus euismod.

Aliquam neque orci, cursus eu eros non, consequat consequat ligula. Sed quis diam ac mauris volutpat imperdiet. Donec vel aliquet eros, sit amet maximus ante. Fusce eu euismod orci. In ultricies dui est, non suscipit massa tempus at. Donec orci purus, vehicula sed pretium sit amet, tincidunt eu ligula. Integer id lorem euismod, commodo velit sed, tincidunt elit.

Vestibulum condimentum et ipsum in feugiat. Praesent a odio volutpat, sollicitudin turpis vel, mattis nulla. Fusce sit amet ante ipsum. Phasellus consectetur varius gravida. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Aenean tempor in magna hendrerit ullamcorper. Morbi elit diam, dapibus eu eros eget, ultrices semper odio. Suspendisse vel massa euismod, porta ipsum accumsan, congue diam. Phasellus elementum tincidunt lacus porttitor venenatis. Praesent ultricies vestibulum velit at consequat. Donec vel augue porta, faucibus risus in, suscipit enim. Quisque tempor vulputate blandit. Nunc mollis sollicitudin nulla in vestibulum.

In hac habitasse platea dictumst. Donec elit augue, mollis ac massa at, laoreet aliquet eros. Pellentesque ultricies auctor lorem, nec tempor lorem fringilla vel. Nullam aliquet, nibh sed vestibulum porttitor, leo leo tincidunt orci, eu eleifend est arcu hendrerit nisi. Nulla tempus leo felis, eu interdum ipsum facilisis et. Sed eu tortor eget nisi iaculis malesuada. Maecenas rutrum cursus feugiat. Nulla facilisi. Integer bibendum lacus justo, ut accumsan sapien tincidunt vitae. Duis vel dolor tellus. Vestibulum quis est quam.

Pellentesque ornare turpis gravida dui consectetur commodo. Mauris rutrum enim a aliquam euismod. Donec in ultricies augue, a pulvinar ligula. Sed at vehicula ipsum, fermentum iaculis justo. Phasellus cursus quis purus ut hendrerit. Duis volutpat sodales ultrices. Donec odio neque, mollis et quam id, euismod auctor quam. Fusce elementum sem justo, ac sagittis dolor commodo et.

Nam eu convallis augue. Integer id tincidunt arcu, eu tempus diam. Maecenas semper suscipit augue ac elementum. Donec mollis lacus ut est efficitur luctus. Interdum et malesuada fames ac ante ipsum primis in faucibus. Mauris mattis ante id tempus tincidunt. Nam lacinia velit tempus suscipit ultricies. Nulla maximus dignissim elit ut interdum. Nulla porttitor dignissim dolor, eget condimentum metus finibus non. Fusce et odio nec quam molestie dapibus. Mauris maximus felis nec ex cursus aliquet. Proin luctus, nunc commodo dictum sollicitudin, velit augue mollis odio, id pretium ex velit placerat diam. Sed tincidunt tincidunt lacus vitae rutrum. Sed cursus diam et augue sollicitudin, sit amet tempus odio maximus.

Nulla facilisi. Proin at facilisis eros. Etiam tristique justo neque, at elementum diam posuere at. Maecenas pulvinar tincidunt fringilla. Ut consequat non ligula sit amet efficitur. Phasellus quis ante molestie, ultricies ipsum in, tempor est. Donec felis risus, finibus eget facilisis vel, porttitor ac erat. Etiam egestas suscipit felis, at laoreet libero cursus non. Nam porttitor, elit id euismod malesuada, purus enim porta justo, nec pulvinar nunc justo ut odio. Vivamus cursus congue nisl vitae condimentum. Pellentesque id tortor malesuada, porta lacus sit amet, dignissim ante. Praesent vulputate orci fermentum justo mollis, eu tincidunt erat pharetra.

Nam finibus varius lacus, sed rhoncus nulla varius quis. Aliquam id viverra risus, ut mollis eros. Nulla sed felis sit amet justo cursus feugiat. Maecenas tempor velit id elit laoreet, vel cursus magna commodo. Aliquam tempor accumsan erat at volutpat. Duis malesuada pharetra luctus. Cras ut metus ut diam finibus tincidunt. Cras in interdum nibh. Nunc ut efficitur nibh. Quisque convallis lacus at efficitur molestie. Sed felis tellus, bibendum a velit vitae, vestibulum fermentum tellus. Vestibulum lacinia turpis nec bibendum tincidunt. Suspendisse non consectetur risus. Sed facilisis ultrices diam, ut blandit arcu consequat in.

Maecenas id mollis ipsum. Phasellus purus nisl, blandit sed orci eget, placerat vulputate urna. In ultricies risus lacus, a vehicula metus posuere quis. Donec sed risus tincidunt, ullamcorper sem ac, fermentum tortor. Aenean in turpis sapien. Proin et lacus nunc. Proin vitae ligula vitae enim volutpat mollis eget ac augue. Sed rutrum lacus in lacus aliquet, at feugiat velit pretium.

Etiam elit lacus, volutpat at tortor a, pharetra pretium quam. Vestibulum a hendrerit tortor. Integer tincidunt vehicula neque, vel finibus tellus scelerisque et. Suspendisse lorem velit, feugiat nec ornare viverra, porttitor a justo. Vivamus a massa porta, ultrices arcu ut, lacinia ligula. Duis laoreet eleifend turpis. Integer vitae justo a eros consequat dignissim.

Mauris felis justo, vehicula imperdiet placerat a, pellentesque at ipsum. Nulla vehicula rhoncus euismod. Sed et odio eros. Integer hendrerit in purus at placerat. Praesent tincidunt ligula vel lectus laoreet elementum. Quisque mi elit, rutrum in eros nec, ornare congue nulla. Sed sit amet eros eros. Sed tortor nisl, sodales vel eleifend in, tempor ac ligula.

Vestibulum vehicula odio magna, vitae rutrum risus rhoncus ut. Nullam vel condimentum purus. Maecenas non nunc efficitur, imperdiet urna et, aliquet dolor. Quisque ex ipsum, congue vel imperdiet sit amet, varius sagittis justo. Nulla convallis, tellus nec mollis blandit, felis odio dapibus nunc, ac vulputate orci tellus id metus. Nulla dapibus pulvinar mi at tincidunt. Integer vel quam sed neque vulputate luctus. Sed at risus quam. Phasellus non augue eu mauris ultrices ullamcorper. Fusce a cursus diam, et blandit mauris. Morbi a ex sed augue feugiat tempor sed non felis. Quisque quis convallis velit. In posuere orci a magna viverra, id euismod lectus rhoncus. Aliquam aliquam imperdiet dolor id aliquet. Nunc augue diam, convallis quis tincidunt nec, semper vel nisi.

Etiam vel ipsum vitae velit tincidunt venenatis. Ut nec iaculis enim. Donec diam metus, fringilla at dapibus a, condimentum et ipsum. Donec mollis quam vel ullamcorper varius. Sed id ex eu massa hendrerit varius ut quis justo. Vestibulum semper nunc purus, eu scelerisque tellus euismod ac. Etiam eget nisi consectetur, maximus odio non, elementum est. Pellentesque in porta libero. Suspendisse accumsan nisl et lorem ullamcorper, vitae tristique odio dictum. Proin malesuada ut risus ut luctus.

Cras neque est, elementum eget iaculis vitae, luctus volutpat libero. Aliquam sollicitudin enim dui, quis congue leo tincidunt vitae. Integer posuere in orci eu ornare. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Maecenas suscipit at leo quis rhoncus. Nullam aliquet nisl mattis lectus volutpat, ut tempor lectus sodales. Sed et porta enim, quis cursus ex. Proin ac velit feugiat, fermentum arcu vel, pharetra elit. Donec viverra mi est. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed justo eros, mattis sed porttitor hendrerit, ultricies a nunc. Aenean posuere turpis justo, in pulvinar nulla maximus at. In bibendum imperdiet libero mattis posuere. Aliquam rhoncus elit nec ultricies auctor.

Vivamus at risus tortor. Quisque consectetur, metus sed imperdiet varius, magna erat lacinia neque, a semper sapien risus quis ipsum. Duis volutpat sed tortor ut suscipit. Aliquam in turpis ut magna viverra egestas at non nunc. Cras viverra est lacus, id laoreet massa porttitor semper. Vivamus accumsan volutpat ex ac efficitur. Aliquam erat volutpat. Nunc posuere ex orci, eu consequat turpis efficitur tincidunt. Donec leo mi, dapibus et finibus et, sodales in nisl. Praesent porta ex sit amet justo ultricies, ut feugiat quam interdum.

Integer nibh elit, mattis id blandit a, facilisis ut ex. Aliquam nec odio odio. Nam quis urna sem. Maecenas et posuere urna, id blandit odio. Duis placerat sodales fermentum. Pellentesque sed commodo nulla, vehicula lobortis quam. Nulla dapibus consectetur aliquam. Praesent ac ipsum eu diam ullamcorper pharetra ut sit amet metus. Quisque facilisis quam lectus, ornare vehicula diam imperdiet et. Nunc pharetra nisl volutpat dui consectetur elementum. Sed id enim elit. Praesent libero ipsum, facilisis id leo eu, blandit tincidunt dui. Nulla sit amet laoreet lacus, vel volutpat felis. Sed varius erat eget magna aliquam vulputate. Mauris elementum elit quis magna mattis, sit amet pretium dolor dignissim. Proin quis quam dapibus quam tincidunt porta at eu sem.

Integer lectus ante, ultrices nec lacus ac, molestie laoreet leo. Pellentesque justo turpis, laoreet laoreet elit id, vulputate accumsan nunc. Ut bibendum dui a felis bibendum tincidunt. Quisque sed lobortis nunc. Maecenas luctus sapien sed turpis semper venenatis. Aenean bibendum blandit velit sed sollicitudin. Aenean ut est purus. Quisque rhoncus turpis euismod finibus cursus. Nam ut eros mauris. Maecenas varius venenatis augue.

Maecenas eget varius dolor. Quisque arcu eros, commodo non fermentum non, bibendum quis nulla. Praesent sed porttitor mi, nec sollicitudin augue. Donec vel ex in elit vulputate volutpat euismod nec ipsum. Aenean mollis lectus et sem vehicula, nec maximus sapien euismod. Donec ut justo at sapien convallis tempus. Proin sagittis diam quis augue ultrices, vitae suscipit ligula malesuada. Aenean vulputate nunc quis laoreet aliquam. Praesent efficitur eros diam, vel iaculis magna convallis et. Pellentesque nec sodales sem. Sed hendrerit turpis sed luctus euismod. Sed dapibus tortor lorem, sit amet laoreet ipsum sollicitudin id. Nunc massa dui, pretium nec purus id, pulvinar egestas libero. Maecenas ullamcorper quam et massa interdum, et efficitur ex ultricies.

Donec cursus elit at odio bibendum dapibus. Maecenas finibus posuere ex ac vehicula. Etiam cursus pellentesque quam, nec semper ligula porttitor tincidunt. Aliquam eleifend mi tortor, eu blandit odio consectetur a. Mauris sed neque quis est feugiat vehicula at in ante. Nunc imperdiet tempus nisi non aliquet. Aenean blandit quam interdum sem pharetra fermentum. Donec consequat, nunc a imperdiet faucibus, justo odio condimentum massa, ac efficitur magna magna ut purus. Mauris a sem lacinia, interdum ligula id, tincidunt urna. Interdum et malesuada fames ac ante ipsum primis in faucibus.

Aenean interdum augue in lacus bibendum venenatis. Aenean molestie massa nisl, sed feugiat orci suscipit non. Donec tortor erat, congue sed pretium posuere, auctor at justo. In eleifend blandit dolor elementum feugiat. Aenean ullamcorper, lacus vestibulum mollis mollis, tellus quam lobortis ipsum, a vulputate nibh purus sed lacus. Donec arcu erat, consectetur at ex ut, pulvinar tincidunt nulla. Curabitur eu molestie risus, nec convallis lectus. Aliquam nec aliquet ipsum. Maecenas aliquet augue vel dapibus ultrices.

Proin vehicula lorem sed risus tempus, sit amet cursus erat porttitor. Cras imperdiet lacus a ipsum pharetra maximus. Proin vel scelerisque erat. In lacinia ornare turpis nec pretium. Fusce quis dui eget neque sollicitudin sagittis. Donec tincidunt risus eget pharetra elementum. Phasellus eget sapien pellentesque, dictum orci sit amet, ornare felis. Curabitur in lectus interdum, rutrum ante non, sollicitudin erat.

Etiam urna sapien, lacinia non sem a, accumsan varius ante. Maecenas nec commodo ante, ut dictum felis. Donec accumsan lectus ut diam fringilla, a viverra odio elementum. Nam nulla nulla, congue sagittis ornare venenatis, placerat vitae metus. Morbi in ante semper, tempus leo ac, scelerisque ipsum. Interdum et malesuada fames ac ante ipsum primis in faucibus. Sed quis auctor nulla. Morbi pretium nunc eu lacus sollicitudin, a ultricies magna consectetur. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Vivamus ex arcu, mollis vitae enim eget, volutpat tempor massa. Curabitur arcu tellus, interdum eget consequat sit amet, sagittis ut justo.

Donec maximus sapien felis, eu rutrum sapien rhoncus vel. Interdum et malesuada fames ac ante ipsum primis in faucibus. Fusce consectetur mattis eros eget dapibus. Nam nunc nulla, commodo vitae porta sed, varius eget ipsum. Praesent eget mi tincidunt, ullamcorper diam in, faucibus dolor. Maecenas justo turpis, volutpat sed neque nec, scelerisque venenatis diam. Morbi dui urna, consectetur nec velit nec, sodales fermentum turpis. Duis in pulvinar sem. Nullam pulvinar eros a ipsum consequat volutpat. Ut ultrices laoreet leo id ultrices. Duis blandit feugiat lorem, vel aliquam purus dignissim sit amet. Phasellus dapibus, ante in sodales commodo, nisl felis sagittis orci, ac imperdiet urna elit at elit. Nullam tincidunt tellus placerat, eleifend velit vel, efficitur tortor. Nullam consequat sapien at metus tincidunt malesuada.

Pellentesque pellentesque a mi nec auctor. Morbi eleifend dui eu enim venenatis facilisis. Vivamus nec lectus congue, ultrices nisl sit amet, pharetra sem. Curabitur blandit tristique ligula sed scelerisque. Quisque ac massa ligula. Nullam venenatis leo nec enim blandit ullamcorper. Nunc euismod sagittis facilisis. Nam tortor ante, placerat non mi vel, dignissim ullamcorper nunc. Praesent accumsan rutrum nisi, congue maximus ipsum sodales ut. Morbi et bibendum lacus.

Nullam malesuada ante eget velit elementum, in dignissim turpis ultrices. Nullam ullamcorper scelerisque dui sed rhoncus. Duis suscipit bibendum urna eget congue. Phasellus sem augue, aliquet posuere varius dictum, pharetra ac leo. Phasellus vel tellus nec nisl ultrices blandit. Morbi nulla justo, bibendum a sagittis vel, maximus et libero. Ut fermentum purus enim, non dictum elit malesuada sed.

Vestibulum scelerisque sit amet tellus a consectetur. Aliquam erat volutpat. Integer volutpat id lorem id tristique. Aliquam suscipit ante ut mauris tincidunt finibus. Nunc vel diam vitae nisi venenatis hendrerit sit amet sit amet purus. Sed molestie, enim et consectetur rhoncus, augue arcu pellentesque eros, vel ultrices velit nisi quis mi. Ut luctus enim finibus nunc tempus, a gravida ipsum luctus. Suspendisse turpis tortor, placerat sit amet felis vitae, ullamcorper bibendum sapien. Nulla facilisi. Ut suscipit sed tortor vel accumsan. Praesent ullamcorper purus ex, et placerat mauris cursus aliquam. Cras in justo a sem pellentesque sodales. Cras venenatis tempus felis.

Nam faucibus lectus sed pharetra porttitor. Curabitur felis libero, feugiat eget commodo nec, molestie eget nunc. Vestibulum dignissim elit sed nisi tempor, eget mattis diam sodales. Suspendisse dictum tortor non felis varius condimentum. Ut semper fermentum accumsan. Fusce pharetra sollicitudin eros, et pharetra metus convallis ac. Nulla iaculis vitae odio ac feugiat. Suspendisse ultricies ex enim, vel varius risus ullamcorper a. Nunc aliquam, lectus nec bibendum efficitur, odio ipsum semper magna, id vestibulum augue tellus eu risus. Maecenas a sem aliquam, sagittis augue sed, lobortis erat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Aenean consequat varius massa eget dignissim. Phasellus egestas lacus quis ultricies aliquet. Aenean quam felis, fermentum vel nisi ac, laoreet gravida nulla.

Mauris scelerisque et ante in porta. Mauris rutrum dui vulputate sagittis luctus. Integer ut diam ac eros luctus fringilla ut eget magna. Quisque nec ipsum nec ligula cursus feugiat. Quisque mattis mi ut elit convallis pretium. Nunc sed aliquam eros. Suspendisse felis enim, mattis eget tincidunt eu, eleifend id augue. Vivamus diam elit, malesuada vel lacus in, iaculis posuere erat.

Proin non risus fermentum, ornare dolor nec, malesuada lorem. Nam a enim ac arcu pellentesque semper. Nullam suscipit porttitor dolor. Nam eget lectus id neque laoreet viverra. Fusce cursus, leo at pharetra eleifend, dui risus scelerisque neque, id pretium est est vitae nisi. Curabitur sed placerat velit, nec ullamcorper nisi. Etiam faucibus libero ultricies libero congue, vitae posuere sapien rutrum. Praesent sed pellentesque metus, ut volutpat risus. Etiam ut vehicula quam, non commodo est. Quisque gravida accumsan eros, id aliquam est dictum nec.

Aliquam id egestas neque. Sed accumsan justo magna, sed ultricies ante placerat nec. Morbi eget lacus tristique, gravida dolor in, volutpat dui. Donec malesuada eros ac elit luctus condimentum. Vivamus id lorem eget leo aliquet congue sagittis eu arcu. Integer sit amet nisl ut massa ultricies laoreet. Pellentesque porta purus in massa facilisis, a auctor nulla molestie. Donec et sapien maximus, euismod ipsum id, sagittis lorem. Ut gravida purus est, at suscipit ipsum varius at. Phasellus ac orci in urna vulputate pharetra.

Nulla ut ex diam. Morbi nec enim enim. In tempus tellus sit amet bibendum molestie. Sed at nibh quis ipsum vestibulum ornare sit amet vel lectus. Nam vulputate non arcu eu lacinia. Maecenas vulputate justo sed dolor rhoncus laoreet. Quisque nunc urna, tempor sit amet odio vitae, porttitor pharetra tellus. Vivamus et quam tristique augue gravida dictum non sit amet arcu. Donec elementum, arcu quis ornare blandit, nisl ex vehicula libero, vel congue mi augue vel massa.

Duis rutrum, tellus placerat lacinia varius, metus nunc gravida tellus, faucibus malesuada odio enim quis mauris. Sed vitae lectus id lorem dapibus imperdiet quis ut neque. Quisque ullamcorper tellus id ipsum cursus sollicitudin. Sed ornare eleifend dictum. Duis nec egestas quam. Etiam ullamcorper massa quis quam euismod fermentum. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. In lacinia varius vulputate. In hac habitasse platea dictumst. Pellentesque venenatis lobortis nibh, quis commodo elit. Cras in ante vehicula, imperdiet dui non, interdum quam.

Integer mattis erat varius mollis pellentesque. Praesent blandit rhoncus malesuada. Vestibulum vel nibh ullamcorper quam sagittis efficitur a id mauris. Ut ultricies ante erat, eu congue urna maximus ut. Donec sed accumsan sapien. Integer diam eros, gravida non facilisis sed, egestas eu diam. Interdum et malesuada fames ac ante ipsum primis in faucibus. Vestibulum in tempor urna. Aenean ultricies nulla mi. Maecenas velit erat, volutpat in augue non, viverra ornare velit. Sed ac gravida ex. Vivamus auctor dui sapien. Nulla ut pretium ipsum, non congue ante.

Nunc mattis lectus in congue aliquet. Nulla sed nisl commodo, vehicula orci non, congue ligula. Vestibulum imperdiet convallis risus, ut tempor eros facilisis id. Ut rutrum, libero eget rhoncus blandit, ante velit tincidunt dolor, ac placerat eros ligula ut sapien. Duis eget erat erat. Aliquam vitae nibh tellus. Suspendisse vitae varius tortor. Fusce ac luctus mauris.

Suspendisse volutpat rutrum purus, sed porta turpis pellentesque in. Aliquam rhoncus mi egestas egestas sollicitudin. Nam nisl lacus, pharetra non scelerisque eu, pharetra sit amet diam. Nam quis nisl eu mauris sollicitudin bibendum id et orci. Morbi dictum urna quis sodales pretium. Suspendisse potenti. Mauris sed leo condimentum, cursus felis non, elementum orci. Suspendisse iaculis, arcu non luctus malesuada, orci dolor varius nisi, sed facilisis nibh eros vel leo. Quisque in facilisis velit. Sed non lorem leo. Nam pretium lectus quis tellus elementum laoreet.

Nunc vitae tempor eros, sed commodo neque. Fusce faucibus ut diam sit amet viverra. Fusce sit amet sollicitudin est, sed auctor mi. Integer mollis eros eu dictum rhoncus. Proin porttitor lobortis aliquam. Nunc sagittis pulvinar lacus ac elementum. Duis diam felis, elementum in iaculis vel, viverra posuere leo. In imperdiet velit odio, eu tristique dui auctor non. Sed maximus sem sit amet sollicitudin finibus. Etiam nec magna pulvinar, auctor est et, sollicitudin tellus. Nunc pharetra nunc vel justo finibus ornare.

Cras aliquet ut ipsum ac tempus. Quisque et mollis tellus, accumsan suscipit velit. Nullam vulputate efficitur mollis. Curabitur suscipit semper erat ac sagittis. Integer non dolor eget purus tristique suscipit at at enim. Aliquam feugiat sed tortor vel consequat. Duis sagittis ante eget arcu cursus luctus. In sed magna at risus porttitor iaculis eget nec libero. Vestibulum iaculis tortor euismod sagittis consequat. Maecenas luctus, mauris ac pulvinar malesuada, est odio finibus arcu, non elementum risus leo in lectus. Donec ut elit at lectus ultrices ornare. Quisque et leo non magna accumsan viverra. Sed eget commodo lacus. Interdum et malesuada fames ac ante ipsum primis in faucibus. Suspendisse tempor, enim eget interdum porttitor, neque augue mattis neque, eu tempus nunc massa sit amet libero. Quisque aliquet placerat neque, quis consectetur libero lobortis nec.

Phasellus et consectetur lorem. Nam porta magna enim, nec molestie ligula iaculis ut. Aliquam interdum efficitur turpis, vitae rutrum tellus ornare aliquam. Phasellus ut pretium lorem. Praesent ut facilisis sapien, non consequat risus. Phasellus et dolor vitae est suscipit auctor. In pharetra vestibulum mi, et pulvinar erat iaculis ut. Maecenas et nisi felis. In nec feugiat risus. In hac habitasse platea dictumst.

Quisque ut nulla a nisi fermentum pharetra. Vivamus vel pharetra diam. Nam eget dolor fringilla libero scelerisque tempus. Vivamus ut scelerisque enim. Aliquam nunc felis, hendrerit in libero eget, elementum sagittis tortor. Vestibulum sagittis, neque ac congue dapibus, neque dolor laoreet ligula, eu mattis arcu erat quis urna. In nunc risus, hendrerit id tortor sed, tempus convallis risus. Morbi faucibus orci vel augue tempus, condimentum maximus ipsum luctus.

Aliquam aliquam luctus risus id tempus. Mauris accumsan tempus ultricies. Vestibulum non nisi eget ipsum convallis semper luctus vel nisi. Cras suscipit tempor dui, vel scelerisque metus mollis et. Mauris et nulla semper, accumsan tortor vitae, tempor odio. Aliquam erat volutpat. Vestibulum vestibulum diam ac dui tincidunt ullamcorper. Proin tempor sed lorem ut mattis. Praesent tempor risus leo, eu commodo arcu blandit eu. Donec tempor felis at rutrum aliquet. Proin bibendum malesuada volutpat. Nulla facilisi.

Morbi finibus bibendum urna pulvinar tempor. Phasellus semper tincidunt leo id interdum. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Curabitur pharetra tellus id ligula pharetra semper. Praesent et tristique erat. Cras luctus, mauris at vestibulum mollis, lacus nibh imperdiet quam, iaculis posuere risus purus eget velit. Duis sed feugiat tellus. Integer convallis rhoncus turpis, vel imperdiet turpis luctus eu. Ut sagittis tortor sed nisl porttitor, quis rhoncus elit suscipit. In lorem massa, lacinia nec pretium id, tempor nec erat. Pellentesque a ante vel nibh placerat aliquet quis et sapien. Fusce vulputate molestie metus, a eleifend ipsum fringilla in.

Donec id condimentum est, sed vestibulum elit. Quisque laoreet nisl sed maximus sodales. Nam ornare tristique ultricies. Aenean porttitor a mauris sit amet rutrum. Aenean a accumsan justo. Duis varius augue in eros rhoncus, in lobortis massa luctus. Nam eu dapibus purus. Pellentesque imperdiet eleifend purus, sit amet lobortis felis tristique et. Donec felis elit, mollis vitae augue vitae, fermentum pretium nunc. Sed tellus felis, tristique at iaculis sed, sagittis non tellus. Aenean convallis tortor eget elit tincidunt vestibulum.

Mauris fringilla tempor lorem eget sollicitudin. Nullam eu lacinia velit. Aenean auctor dui leo, ut tincidunt mi convallis eu. Vivamus rutrum, nibh sed finibus semper, erat leo iaculis erat, ut pharetra metus quam a risus. Nunc aliquam quis nulla non dapibus. Vivamus efficitur semper placerat. Vivamus nec metus nec elit suscipit volutpat. Nam porttitor ipsum ac rutrum convallis. Quisque vulputate est ac justo mollis, eu dignissim ipsum luctus. Morbi eget tincidunt velit. Suspendisse tempor et nunc eu porta. Curabitur quis blandit purus. Etiam ut elit ultricies, euismod libero eget, tincidunt diam.

Phasellus dignissim volutpat nulla, vitae porttitor nulla. Phasellus molestie gravida suscipit. Duis non felis id ipsum varius tincidunt. Aenean sit amet ornare ipsum. Aliquam massa justo, aliquet eu condimentum sed, lobortis ut turpis. Vestibulum vel felis in enim gravida dictum at quis diam. Proin ac consequat est.

Curabitur volutpat eleifend lacus et semper. Morbi quis interdum tellus. Pellentesque scelerisque mollis interdum. Nullam ut feugiat ligula, nec pretium tortor. Etiam semper ornare augue, a consequat erat sodales in. Nulla consequat maximus magna. Ut venenatis ut quam nec consectetur. Curabitur non bibendum est. Aliquam scelerisque erat in fermentum posuere. Curabitur aliquet, nibh ut consequat mattis, urna magna consequat quam, a suscipit enim sapien ut leo. Nam mollis sit amet turpis vel pretium.

Aliquam a tincidunt massa. Aenean interdum elit diam, facilisis malesuada dui lacinia et. Proin porttitor feugiat lorem sed luctus. Donec convallis finibus felis, vitae finibus magna cursus at. Phasellus turpis quam, feugiat ac odio et, gravida ullamcorper nulla. Quisque nulla felis, auctor et pellentesque eget, vestibulum quis massa. Nulla facilisi. Quisque in est sit amet quam ornare rhoncus blandit maximus sem. Duis in ultrices ligula. Nulla commodo tortor sed tempus pretium. Aenean pharetra mi ipsum, sit amet accumsan lacus pellentesque vel. Lorem ipsum dolor sit amet, consectetur adipiscing elit.

In iaculis sem sed aliquam consectetur. Suspendisse et est at leo egestas scelerisque vel in diam. Suspendisse posuere pharetra mollis. Vestibulum facilisis commodo fermentum. Nunc ullamcorper lacinia mi, in iaculis arcu hendrerit id. Ut ullamcorper pulvinar odio eget ultricies. Proin efficitur nisi nec dui scelerisque, ac lobortis ante vestibulum. Nunc ac arcu at metus lobortis convallis eget ac libero. Ut eget massa nec mi aliquam facilisis.

Donec pulvinar efficitur nisl at maximus. Donec massa sapien, scelerisque molestie interdum non, porta vitae dui. Nam at nisi nec sem auctor consequat. Nullam malesuada quis nisl ut aliquam. Phasellus dapibus nibh tortor. Aliquam convallis ut quam id fermentum. Nulla vehicula eget nisi fringilla lacinia. Nullam fringilla volutpat semper. Mauris sodales leo at metus vehicula, eu maximus sem iaculis. Sed euismod nec orci vel condimentum.

Morbi felis velit, ornare nec volutpat a, dapibus vitae odio. Donec eu leo neque. Donec porta, augue ac vehicula tincidunt, augue erat egestas dolor, nec iaculis nibh enim vel ligula. Donec id nibh dictum arcu faucibus eleifend et vitae ex. Sed tincidunt erat dictum, aliquam diam ac, tincidunt erat. Nulla mi tortor, facilisis ut laoreet et, sodales vitae sapien. Curabitur efficitur sollicitudin ipsum ut euismod. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Aenean vel turpis lacinia, aliquet neque et, molestie sapien. Sed imperdiet ultrices accumsan. Aliquam eleifend ullamcorper metus. Fusce id nunc in enim ultrices malesuada. Proin eget nibh sem.

Cras dignissim vestibulum felis, vitae semper justo mollis quis. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nullam laoreet diam ut risus rhoncus, at ultricies lacus sollicitudin. Nam euismod odio aliquam mi molestie rhoncus. Nunc a tortor porta, feugiat nisl id, rutrum enim. Donec vel dignissim arcu. Sed ex tellus, fermentum in ornare at, sollicitudin non eros.
    """
}
    