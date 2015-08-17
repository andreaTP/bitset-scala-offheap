package jmh

import org.openjdk.jmh.annotations._
import scala.offheap._

import java.nio.file.Files.readAllBytes
import java.nio.file.Paths.get

@State(Scope.Thread)
class GCBitmapSearch {
  @Param(scala.Array("10"))
  var n: Int = _

  //data.loadText()

  @Benchmark
  def run = GCAlgo.run(n)
}

@State(Scope.Thread)
class OffheapBitmapSearch {
  @Param(scala.Array("10"))
  var n: Int = _

  //data.loadText()

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
/*
  def loadText() = {
    import java.io._
    import java.util.Scanner

    val s = new Scanner(new BufferedReader(new FileReader(getClass.getResource("/divina_commedia.txt").getPath)))

    val sb = new StringBuilder()
    var str = ""
    while (s.hasNext()) {
      str = s.next()
      sb.append(str)
    }
    text = sb.toString
  }
*/

  val pattern = "facea spiacer suo lezzo."

  var text = """
  Nel mezzo del cammin di nostra vita
  mi ritrovai per una selva oscura,
  ch� la diritta via era smarrita.
  Ahi quanto a dir qual era � cosa dura
  esta selva selvaggia e aspra e forte
  che nel pensier rinova la paura!
  Tant' � amara che poco � pi� morte;
  ma per trattar del ben ch'i' vi trovai,
  dir� de l'altre cose ch'i' v'ho scorte.
  Io non so ben ridir com' i' v'intrai,
  tant' era pien di sonno a quel punto
  che la verace via abbandonai.
  Ma poi ch'i' fui al pi� d'un colle giunto,
  l� dove terminava quella valle
  che m'avea di paura il cor compunto,
  guardai in alto e vidi le sue spalle
  vestite gi� de' raggi del pianeta
  che mena dritto altrui per ogne calle.
  Allor fu la paura un poco queta,
  che nel lago del cor m'era durata
  la notte ch'i' passai con tanta pieta.
  E come quei che con lena affannata,
  uscito fuor del pelago a la riva,
  si volge a l'acqua perigliosa e guata,
  cos� l'animo mio, ch'ancor fuggiva,
  si volse a retro a rimirar lo passo
  che non lasci� gi� mai persona viva.
  Poi ch'�i posato un poco il corpo lasso,
  ripresi via per la piaggia diserta,
  s� che 'l pi� fermo sempre era 'l pi� basso.
  Ed ecco, quasi al cominciar de l'erta,
  una lonza leggiera e presta molto,
  che di pel macolato era coverta;
  e non mi si partia dinanzi al volto,
  anzi 'mpediva tanto il mio cammino,
  ch'i' fui per ritornar pi� volte v�lto.
  Temp' era dal principio del mattino,
  e 'l sol montava 'n s� con quelle stelle
  ch'eran con lui quando l'amor divino
  mosse di prima quelle cose belle;
  s� ch'a bene sperar m'era cagione
  di quella fiera a la gaetta pelle
  l'ora del tempo e la dolce stagione;
  ma non s� che paura non mi desse
  la vista che m'apparve d'un leone.
  Questi parea che contra me venisse
  con la test' alta e con rabbiosa fame,
  s� che parea che l'aere ne tremesse.
  Ed una lupa, che di tutte brame
  sembiava carca ne la sua magrezza,
  e molte genti f� gi� viver grame,
  questa mi porse tanto di gravezza
  con la paura ch'uscia di sua vista,
  ch'io perdei la speranza de l'altezza.
  E qual � quei che volontieri acquista,
  e giugne 'l tempo che perder lo face,
  che 'n tutti suoi pensier piange e s'attrista;
  tal mi fece la bestia sanza pace,
  che, venendomi 'ncontro, a poco a poco
  mi ripigneva l� dove 'l sol tace.
  Mentre ch'i' rovinava in basso loco,
  dinanzi a li occhi mi si fu offerto
  chi per lungo silenzio parea fioco.
  Quando vidi costui nel gran diserto,
  �Miserere di me�, gridai a lui,
  �qual che tu sii, od ombra od omo certo!�.
  Rispuosemi: �Non omo, omo gi� fui,
  e li parenti miei furon lombardi,
  mantoani per patr�a ambedui.
  Nacqui sub Iulio, ancor che fosse tardi,
  e vissi a Roma sotto 'l buono Augusto
  nel tempo de li d�i falsi e bugiardi.
  Poeta fui, e cantai di quel giusto
  figliuol d'Anchise che venne di Troia,
  poi che 'l superbo Il��n fu combusto.
  Ma tu perch� ritorni a tanta noia?
  perch� non sali il dilettoso monte
  ch'� principio e cagion di tutta gioia?�.
  �Or se' tu quel Virgilio e quella fonte
  che spandi di parlar s� largo fiume?�,
  rispuos' io lui con vergognosa fronte.
  �O de li altri poeti onore e lume,
  vagliami 'l lungo studio e 'l grande amore
  che m'ha fatto cercar lo tuo volume.
  Tu se' lo mio maestro e 'l mio autore,
  tu se' solo colui da cu' io tolsi
  lo bello stilo che m'ha fatto onore.
  Vedi la bestia per cu' io mi volsi;
  aiutami da lei, famoso saggio,
  ch'ella mi fa tremar le vene e i polsi�.
  �A te convien tenere altro v�aggio�,
  rispuose, poi che lagrimar mi vide,
  �se vuo' campar d'esto loco selvaggio;
  ch� questa bestia, per la qual tu gride,
  non lascia altrui passar per la sua via,
  ma tanto lo 'mpedisce che l'uccide;
  e ha natura s� malvagia e ria,
  che mai non empie la bramosa voglia,
  e dopo 'l pasto ha pi� fame che pria.
  Molti son li animali a cui s'ammoglia,
  e pi� saranno ancora, infin che 'l veltro
  verr�, che la far� morir con doglia.
  Questi non ciber� terra n� peltro,
  ma sap�enza, amore e virtute,
  e sua nazion sar� tra feltro e feltro.
  Di quella umile Italia fia salute
  per cui mor� la vergine Cammilla,
  Eurialo e Turno e Niso di ferute.
  Questi la caccer� per ogne villa,
  fin che l'avr� rimessa ne lo 'nferno,
  l� onde 'nvidia prima dipartilla.
  Ond' io per lo tuo me' penso e discerno
  che tu mi segui, e io sar� tua guida,
  e trarrotti di qui per loco etterno;
  ove udirai le disperate strida,
  vedrai li antichi spiriti dolenti,
  ch'a la seconda morte ciascun grida;
  e vederai color che son contenti
  nel foco, perch� speran di venire
  quando che sia a le beate genti.
  A le quai poi se tu vorrai salire,
  anima fia a ci� pi� di me degna:
  con lei ti lascer� nel mio partire;
  ch� quello imperador che l� s� regna,
  perch' i' fu' ribellante a la sua legge,
  non vuol che 'n sua citt� per me si vegna.
  In tutte parti impera e quivi regge;
  quivi � la sua citt� e l'alto seggio:
  oh felice colui cu' ivi elegge!�.
  E io a lui: �Poeta, io ti richeggio
  per quello Dio che tu non conoscesti,
  a ci� ch'io fugga questo male e peggio,
  che tu mi meni l� dov' or dicesti,
  s� ch'io veggia la porta di san Pietro
  e color cui tu fai cotanto mesti�.
  Allor si mosse, e io li tenni dietro.

  CANTO II
  [Canto secondo de la prima parte ne la quale fa proemio a la prima cantica cio� a la prima parte di questo libro solamente, e in questo canto tratta l'auttore come trov� Virgilio, il quale il fece sicuro del cammino per le tre donne che di lui aveano cura ne la corte del cielo.]


  Lo giorno se n'andava, e l'aere bruno
  toglieva li animai che sono in terra
  da le fatiche loro; e io sol uno
  m'apparecchiava a sostener la guerra
  s� del cammino e s� de la pietate,
  che ritrarr� la mente che non erra.
  O muse, o alto ingegno, or m'aiutate;
  o mente che scrivesti ci� ch'io vidi,
  qui si parr� la tua nobilitate.
  Io cominciai: �Poeta che mi guidi,
  guarda la mia virt� s'ell' � possente,
  prima ch'a l'alto passo tu mi fidi.
  Tu dici che di Silv�o il parente,
  corruttibile ancora, ad immortale
  secolo and�, e fu sensibilmente.
  Per�, se l'avversario d'ogne male
  cortese i fu, pensando l'alto effetto
  ch'uscir dovea di lui, e 'l chi e 'l quale
  non pare indegno ad omo d'intelletto;
  ch'e' fu de l'alma Roma e di suo impero
  ne l'empireo ciel per padre eletto:
  la quale e 'l quale, a voler dir lo vero,
  fu stabilita per lo loco santo
  u' siede il successor del maggior Piero.
  Per quest' andata onde li dai tu vanto,
  intese cose che furon cagione
  di sua vittoria e del papale ammanto.
  Andovvi poi lo Vas d'elez�one,
  per recarne conforto a quella fede
  ch'� principio a la via di salvazione.
  Ma io, perch� venirvi? o chi 'l concede?
  Io non En�a, io non Paulo sono;
  me degno a ci� n� io n� altri 'l crede.
  Per che, se del venire io m'abbandono,
  temo che la venuta non sia folle.
  Se' savio; intendi me' ch'i' non ragiono�.
  E qual � quei che disvuol ci� che volle
  e per novi pensier cangia proposta,
  s� che dal cominciar tutto si tolle,
  tal mi fec' �o 'n quella oscura costa,
  perch�, pensando, consumai la 'mpresa
  che fu nel cominciar cotanto tosta.
  �S'i' ho ben la parola tua intesa�,
  rispuose del magnanimo quell' ombra,
  �l'anima tua � da viltade offesa;
  la qual molte f�ate l'omo ingombra
  s� che d'onrata impresa lo rivolve,
  come falso veder bestia quand' ombra.
  Da questa tema acci� che tu ti solve,
  dirotti perch' io venni e quel ch'io 'ntesi
  nel primo punto che di te mi dolve.
  Io era tra color che son sospesi,
  e donna mi chiam� beata e bella,
  tal che di comandare io la richiesi.
  Lucevan li occhi suoi pi� che la stella;
  e cominciommi a dir soave e piana,
  con angelica voce, in sua favella:
  "O anima cortese mantoana,
  di cui la fama ancor nel mondo dura,
  e durer� quanto 'l mondo lontana,
  l'amico mio, e non de la ventura,
  ne la diserta piaggia � impedito
  s� nel cammin, che v�lt' � per paura;
  e temo che non sia gi� s� smarrito,
  ch'io mi sia tardi al soccorso levata,
  per quel ch'i' ho di lui nel cielo udito.
  Or movi, e con la tua parola ornata
  e con ci� c'ha mestieri al suo campare,
  l'aiuta s� ch'i' ne sia consolata.
  I' son Beatrice che ti faccio andare;
  vegno del loco ove tornar disio;
  amor mi mosse, che mi fa parlare.
  Quando sar� dinanzi al segnor mio,
  di te mi loder� sovente a lui".
  Tacette allora, e poi comincia' io:
  "O donna di virt� sola per cui
  l'umana spezie eccede ogne contento
  di quel ciel c'ha minor li cerchi sui,
  tanto m'aggrada il tuo comandamento,
  che l'ubidir, se gi� fosse, m'� tardi;
  pi� non t'� uo' ch'aprirmi il tuo talento.
  Ma dimmi la cagion che non ti guardi
  de lo scender qua giuso in questo centro
  de l'ampio loco ove tornar tu ardi".
  "Da che tu vuo' saver cotanto a dentro,
  dirotti brievemente", mi rispuose,
  "perch' i' non temo di venir qua entro.
  Temer si dee di sole quelle cose
  c'hanno potenza di fare altrui male;
  de l'altre no, ch� non son paurose.
  I' son fatta da Dio, sua merc�, tale,
  che la vostra miseria non mi tange,
  n� fiamma d'esto 'ncendio non m'assale.
  Donna � gentil nel ciel che si compiange
  di questo 'mpedimento ov' io ti mando,
  s� che duro giudicio l� s� frange.
  Questa chiese Lucia in suo dimando
  e disse: � Or ha bisogno il tuo fedele
  di te, e io a te lo raccomando �.
  Lucia, nimica di ciascun crudele,
  si mosse, e venne al loco dov' i' era,
  che mi sedea con l'antica Rachele.
  Disse: � Beatrice, loda di Dio vera,
  ch� non soccorri quei che t'am� tanto,
  ch'usc� per te de la volgare schiera?
  Non odi tu la pieta del suo pianto,
  non vedi tu la morte che 'l combatte
  su la fiumana ove 'l mar non ha vanto? �.
  Al mondo non fur mai persone ratte
  a far lor pro o a fuggir lor danno,
  com' io, dopo cotai parole fatte,
  venni qua gi� del mio beato scanno,
  fidandomi del tuo parlare onesto,
  ch'onora te e quei ch'udito l'hanno".
  Poscia che m'ebbe ragionato questo,
  li occhi lucenti lagrimando volse,
  per che mi fece del venir pi� presto.
  E venni a te cos� com' ella volse:
  d'inanzi a quella fiera ti levai
  che del bel monte il corto andar ti tolse.
  Dunque: che �? perch�, perch� restai,
  perch� tanta vilt� nel core allette,
  perch� ardire e franchezza non hai,
  poscia che tai tre donne benedette
  curan di te ne la corte del cielo,
  e 'l mio parlar tanto ben ti promette?�.
  Quali fioretti dal notturno gelo
  chinati e chiusi, poi che 'l sol li 'mbianca,
  si drizzan tutti aperti in loro stelo,
  tal mi fec' io di mia virtude stanca,
  e tanto buono ardire al cor mi corse,
  ch'i' cominciai come persona franca:
  �Oh pietosa colei che mi soccorse!
  e te cortese ch'ubidisti tosto
  a le vere parole che ti porse!
  Tu m'hai con disiderio il cor disposto
  s� al venir con le parole tue,
  ch'i' son tornato nel primo proposto.
  Or va, ch'un sol volere � d'ambedue:
  tu duca, tu segnore e tu maestro�.
  Cos� li dissi; e poi che mosso fue,
  intrai per lo cammino alto e silvestro.

  CANTO III
  [Canto terzo, nel quale tratta de la porta e de l'entrata de l'inferno e del fiume d'Acheronte, de la pena di coloro che vissero sanza opere di fama degne, e come il demonio Caron li trae in sua nave e come elli parl� a l'auttore; e tocca qui questo vizio ne la persona di papa Cilestino.]


  'Per me si va ne la citt� dolente,
  per me si va ne l'etterno dolore,
  per me si va tra la perduta gente.
  Giustizia mosse il mio alto fattore;
  fecemi la divina podestate,
  la somma sap�enza e 'l primo amore.
  Dinanzi a me non fuor cose create
  se non etterne, e io etterno duro.
  Lasciate ogne speranza, voi ch'intrate'.
  Queste parole di colore oscuro
  vid' �o scritte al sommo d'una porta;
  per ch'io: �Maestro, il senso lor m'� duro�.
  Ed elli a me, come persona accorta:
  �Qui si convien lasciare ogne sospetto;
  ogne vilt� convien che qui sia morta.
  Noi siam venuti al loco ov' i' t'ho detto
  che tu vedrai le genti dolorose
  c'hanno perduto il ben de l'intelletto�.
  E poi che la sua mano a la mia puose
  con lieto volto, ond' io mi confortai,
  mi mise dentro a le segrete cose.
  Quivi sospiri, pianti e alti guai
  risonavan per l'aere sanza stelle,
  per ch'io al cominciar ne lagrimai.
  Diverse lingue, orribili favelle,
  parole di dolore, accenti d'ira,
  voci alte e fioche, e suon di man con elle
  facevano un tumulto, il qual s'aggira
  sempre in quell' aura sanza tempo tinta,
  come la rena quando turbo spira.
  E io ch'avea d'error la testa cinta,
  dissi: �Maestro, che � quel ch'i' odo?
  e che gent' � che par nel duol s� vinta?�.
  Ed elli a me: �Questo misero modo
  tegnon l'anime triste di coloro
  che visser sanza 'nfamia e sanza lodo.
  Mischiate sono a quel cattivo coro
  de li angeli che non furon ribelli
  n� fur fedeli a Dio, ma per s� fuoro.
  Caccianli i ciel per non esser men belli,
  n� lo profondo inferno li riceve,
  ch'alcuna gloria i rei avrebber d'elli�.
  E io: �Maestro, che � tanto greve
  a lor che lamentar li fa s� forte?�.
  Rispuose: �Dicerolti molto breve.
  Questi non hanno speranza di morte,
  e la lor cieca vita � tanto bassa,
  che 'nvid�osi son d'ogne altra sorte.
  Fama di loro il mondo esser non lassa;
  misericordia e giustizia li sdegna:
  non ragioniam di lor, ma guarda e passa�.
  E io, che riguardai, vidi una 'nsegna
  che girando correva tanto ratta,
  che d'ogne posa mi parea indegna;
  e dietro le ven�a s� lunga tratta
  di gente, ch'i' non averei creduto
  che morte tanta n'avesse disfatta.
  Poscia ch'io v'ebbi alcun riconosciuto,
  vidi e conobbi l'ombra di colui
  che fece per viltade il gran rifiuto.
  Incontanente intesi e certo fui
  che questa era la setta d'i cattivi,
  a Dio spiacenti e a' nemici sui.
  Questi sciaurati, che mai non fur vivi,
  erano ignudi e stimolati molto
  da mosconi e da vespe ch'eran ivi.
  Elle rigavan lor di sangue il volto,
  che, mischiato di lagrime, a' lor piedi
  da fastidiosi vermi era ricolto.
  E poi ch'a riguardar oltre mi diedi,
  vidi genti a la riva d'un gran fiume;
  per ch'io dissi: �Maestro, or mi concedi
  ch'i' sappia quali sono, e qual costume
  le fa di trapassar parer s� pronte,
  com' i' discerno per lo fioco lume�.
  Ed elli a me: �Le cose ti fier conte
  quando noi fermerem li nostri passi
  su la trista riviera d'Acheronte�.
  Allor con li occhi vergognosi e bassi,
  temendo no 'l mio dir li fosse grave,
  infino al fiume del parlar mi trassi.
  Ed ecco verso noi venir per nave
  un vecchio, bianco per antico pelo,
  gridando: �Guai a voi, anime prave!
  Non isperate mai veder lo cielo:
  i' vegno per menarvi a l'altra riva
  ne le tenebre etterne, in caldo e 'n gelo.
  E tu che se' cost�, anima viva,
  p�rtiti da cotesti che son morti�.
  Ma poi che vide ch'io non mi partiva,
  disse: �Per altra via, per altri porti
  verrai a piaggia, non qui, per passare:
  pi� lieve legno convien che ti porti�.
  E 'l duca lui: �Caron, non ti crucciare:
  vuolsi cos� col� dove si puote
  ci� che si vuole, e pi� non dimandare�.
  Quinci fuor quete le lanose gote
  al nocchier de la livida palude,
  che 'ntorno a li occhi avea di fiamme rote.
  Ma quell' anime, ch'eran lasse e nude,
  cangiar colore e dibattero i denti,
  ratto che 'nteser le parole crude.
  Bestemmiavano Dio e lor parenti,
  l'umana spezie e 'l loco e 'l tempo e 'l seme
  di lor semenza e di lor nascimenti.
  Poi si ritrasser tutte quante insieme,
  forte piangendo, a la riva malvagia
  ch'attende ciascun uom che Dio non teme.
  Caron dimonio, con occhi di bragia
  loro accennando, tutte le raccoglie;
  batte col remo qualunque s'adagia.
  Come d'autunno si levan le foglie
  l'una appresso de l'altra, fin che 'l ramo
  vede a la terra tutte le sue spoglie,
  similemente il mal seme d'Adamo
  gittansi di quel lito ad una ad una,
  per cenni come augel per suo richiamo.
  Cos� sen vanno su per l'onda bruna,
  e avanti che sien di l� discese,
  anche di qua nuova schiera s'auna.
  �Figliuol mio�, disse 'l maestro cortese,
  �quelli che muoion ne l'ira di Dio
  tutti convegnon qui d'ogne paese;
  e pronti sono a trapassar lo rio,
  ch� la divina giustizia li sprona,
  s� che la tema si volve in disio.
  Quinci non passa mai anima buona;
  e per�, se Caron di te si lagna,
  ben puoi sapere omai che 'l suo dir suona�.
  Finito questo, la buia campagna
  trem� s� forte, che de lo spavento
  la mente di sudore ancor mi bagna.
  La terra lagrimosa diede vento,
  che balen� una luce vermiglia
  la qual mi vinse ciascun sentimento;
  e caddi come l'uom cui sonno piglia.

  CANTO IV
  [Canto quarto, nel quale mostra del primo cerchio de l'inferno, luogo detto Limbo, e quivi tratta de la pena de' non battezzati e de' valenti uomini, li quali moriron innanzi l'avvenimento di Ges� Cristo e non conobbero debitamente Idio; e come Ies� Cristo trasse di questo luogo molte anime.]


  Ruppemi l'alto sonno ne la testa
  un greve truono, s� ch'io mi riscossi
  come persona ch'� per forza desta;
  e l'occhio riposato intorno mossi,
  dritto levato, e fiso riguardai
  per conoscer lo loco dov' io fossi.
  Vero � che 'n su la proda mi trovai
  de la valle d'abisso dolorosa
  che 'ntrono accoglie d'infiniti guai.
  Oscura e profonda era e nebulosa
  tanto che, per ficcar lo viso a fondo,
  io non vi discernea alcuna cosa.
  �Or discendiam qua gi� nel cieco mondo�,
  cominci� il poeta tutto smorto.
  �Io sar� primo, e tu sarai secondo�.
  E io, che del color mi fui accorto,
  dissi: �Come verr�, se tu paventi
  che suoli al mio dubbiare esser conforto?�.
  Ed elli a me: �L'angoscia de le genti
  che son qua gi�, nel viso mi dipigne
  quella piet� che tu per tema senti.
  Andiam, ch� la via lunga ne sospigne�.
  Cos� si mise e cos� mi f� intrare
  nel primo cerchio che l'abisso cigne.
  Quivi, secondo che per ascoltare,
  non avea pianto mai che di sospiri
  che l'aura etterna facevan tremare;
  ci� avvenia di duol sanza mart�ri,
  ch'avean le turbe, ch'eran molte e grandi,
  d'infanti e di femmine e di viri.
  Lo buon maestro a me: �Tu non dimandi
  che spiriti son questi che tu vedi?
  Or vo' che sappi, innanzi che pi� andi,
  ch'ei non peccaro; e s'elli hanno mercedi,
  non basta, perch� non ebber battesmo,
  ch'� porta de la fede che tu credi;
  e s'e' furon dinanzi al cristianesmo,
  non adorar debitamente a Dio:
  e di questi cotai son io medesmo.
  Per tai difetti, non per altro rio,
  semo perduti, e sol di tanto offesi
  che sanza speme vivemo in disio�.
  Gran duol mi prese al cor quando lo 'ntesi,
  per� che gente di molto valore
  conobbi che 'n quel limbo eran sospesi.
  �Dimmi, maestro mio, dimmi, segnore�,
  comincia' io per volere esser certo
  di quella fede che vince ogne errore:
  �uscicci mai alcuno, o per suo merto
  o per altrui, che poi fosse beato?�.
  E quei che 'ntese il mio parlar coverto,
  rispuose: �Io era nuovo in questo stato,
  quando ci vidi venire un possente,
  con segno di vittoria coronato.
  Trasseci l'ombra del primo parente,
  d'Ab�l suo figlio e quella di No�,
  di Mo�s� legista e ubidente;
  Abra�m patr�arca e Dav�d re,
  Isra�l con lo padre e co' suoi nati
  e con Rachele, per cui tanto f�,
  e altri molti, e feceli beati.
  E vo' che sappi che, dinanzi ad essi,
  spiriti umani non eran salvati�.
  Non lasciavam l'andar perch' ei dicessi,
  ma passavam la selva tuttavia,
  la selva, dico, di spiriti spessi.
  Non era lunga ancor la nostra via
  di qua dal sonno, quand' io vidi un foco
  ch'emisperio di tenebre vincia.
  Di lungi n'eravamo ancora un poco,
  ma non s� ch'io non discernessi in parte
  ch'orrevol gente possedea quel loco.
  �O tu ch'onori sc�enz�a e arte,
  questi chi son c'hanno cotanta onranza,
  che dal modo de li altri li diparte?�.
  E quelli a me: �L'onrata nominanza
  che di lor suona s� ne la tua vita,
  graz�a acquista in ciel che s� li avanza�.
  Intanto voce fu per me udita:
  �Onorate l'altissimo poeta;
  l'ombra sua torna, ch'era dipartita�.
  Poi che la voce fu restata e queta,
  vidi quattro grand' ombre a noi venire:
  sembianz' avevan n� trista n� lieta.
  Lo buon maestro cominci� a dire:
  �Mira colui con quella spada in mano,
  che vien dinanzi ai tre s� come sire:
  quelli � Omero poeta sovrano;
  l'altro � Orazio satiro che vene;
  Ovidio � 'l terzo, e l'ultimo Lucano.
  Per� che ciascun meco si convene
  nel nome che son� la voce sola,
  fannomi onore, e di ci� fanno bene�.
  Cos� vid' i' adunar la bella scola
  di quel segnor de l'altissimo canto
  che sovra li altri com' aquila vola.
  Da ch'ebber ragionato insieme alquanto,
  volsersi a me con salutevol cenno,
  e 'l mio maestro sorrise di tanto;
  e pi� d'onore ancora assai mi fenno,
  ch'e' s� mi fecer de la loro schiera,
  s� ch'io fui sesto tra cotanto senno.
  Cos� andammo infino a la lumera,
  parlando cose che 'l tacere � bello,
  s� com' era 'l parlar col� dov' era.
  Venimmo al pi� d'un nobile castello,
  sette volte cerchiato d'alte mura,
  difeso intorno d'un bel fiumicello.
  Questo passammo come terra dura;
  per sette porte intrai con questi savi:
  giugnemmo in prato di fresca verdura.
  Genti v'eran con occhi tardi e gravi,
  di grande autorit� ne' lor sembianti:
  parlavan rado, con voci soavi.
  Traemmoci cos� da l'un de' canti,
  in loco aperto, luminoso e alto,
  s� che veder si potien tutti quanti.
  Col� diritto, sovra 'l verde smalto,
  mi fuor mostrati li spiriti magni,
  che del vedere in me stesso m'essalto.
  I' vidi Eletra con molti compagni,
  tra ' quai conobbi Ett�r ed Enea,
  Cesare armato con li occhi grifagni.
  Vidi Cammilla e la Pantasilea;
  da l'altra parte vidi 'l re Latino
  che con Lavina sua figlia sedea.
  Vidi quel Bruto che cacci� Tarquino,
  Lucrezia, Iulia, Marz�a e Corniglia;
  e solo, in parte, vidi 'l Saladino.
  Poi ch'innalzai un poco pi� le ciglia,
  vidi 'l maestro di color che sanno
  seder tra filosofica famiglia.
  Tutti lo miran, tutti onor li fanno:
  quivi vid' �o Socrate e Platone,
  che 'nnanzi a li altri pi� presso li stanno;
  Democrito che 'l mondo a caso pone,
  D�ogen�s, Anassagora e Tale,
  Empedocl�s, Eraclito e Zenone;
  e vidi il buono accoglitor del quale,
  D�ascoride dico; e vidi Orfeo,
  Tul�o e Lino e Seneca morale;
  Euclide geom�tra e Tolomeo,
  Ipocr�te, Avicenna e Gal�eno,
  Avero�s, che 'l gran comento feo.
  Io non posso ritrar di tutti a pieno,
  per� che s� mi caccia il lungo tema,
  che molte volte al fatto il dir vien meno.
  La sesta compagnia in due si scema:
  per altra via mi mena il savio duca,
  fuor de la queta, ne l'aura che trema.
  E vegno in parte ove non � che luca.

  CANTO V
  [Canto quinto, nel quale mostra del secondo cerchio de l'inferno, e tratta de la pena del vizio de la lussuria ne la persona di pi� famosi gentili uomini.]


  Cos� discesi del cerchio primaio
  gi� nel secondo, che men loco cinghia
  e tanto pi� dolor, che punge a guaio.
  Stavvi Min�s orribilmente, e ringhia:
  essamina le colpe ne l'intrata;
  giudica e manda secondo ch'avvinghia.
  Dico che quando l'anima mal nata
  li vien dinanzi, tutta si confessa;
  e quel conoscitor de le peccata
  vede qual loco d'inferno � da essa;
  cignesi con la coda tante volte
  quantunque gradi vuol che gi� sia messa.
  Sempre dinanzi a lui ne stanno molte:
  vanno a vicenda ciascuna al giudizio,
  dicono e odono e poi son gi� volte.
  �O tu che vieni al doloroso ospizio�,
  disse Min�s a me quando mi vide,
  lasciando l'atto di cotanto offizio,
  �guarda com' entri e di cui tu ti fide;
  non t'inganni l'ampiezza de l'intrare!�.
  E 'l duca mio a lui: �Perch� pur gride?
  Non impedir lo suo fatale andare:
  vuolsi cos� col� dove si puote
  ci� che si vuole, e pi� non dimandare�.
  Or incomincian le dolenti note
  a farmisi sentire; or son venuto
  l� dove molto pianto mi percuote.
  Io venni in loco d'ogne luce muto,
  che mugghia come fa mar per tempesta,
  se da contrari venti � combattuto.
  La bufera infernal, che mai non resta,
  mena li spirti con la sua rapina;
  voltando e percotendo li molesta.
  Quando giungon davanti a la ruina,
  quivi le strida, il compianto, il lamento;
  bestemmian quivi la virt� divina.
  Intesi ch'a cos� fatto tormento
  enno dannati i peccator carnali,
  che la ragion sommettono al talento.
  E come li stornei ne portan l'ali
  nel freddo tempo, a schiera larga e piena,
  cos� quel fiato li spiriti mali
  di qua, di l�, di gi�, di s� li mena;
  nulla speranza li conforta mai,
  non che di posa, ma di minor pena.
  E come i gru van cantando lor lai,
  faccendo in aere di s� lunga riga,
  cos� vid' io venir, traendo guai,
  ombre portate da la detta briga;
  per ch'i' dissi: �Maestro, chi son quelle
  genti che l'aura nera s� gastiga?�.
  �La prima di color di cui novelle
  tu vuo' saper�, mi disse quelli allotta,
  �fu imperadrice di molte favelle.
  A vizio di lussuria fu s� rotta,
  che libito f� licito in sua legge,
  per t�rre il biasmo in che era condotta.
  Ell' � Semiram�s, di cui si legge
  che succedette a Nino e fu sua sposa:
  tenne la terra che 'l Soldan corregge.
  L'altra � colei che s'ancise amorosa,
  e ruppe fede al cener di Sicheo;
  poi � Cleopatr�s lussur�osa.
  Elena vedi, per cui tanto reo
  tempo si volse, e vedi 'l grande Achille,
  che con amore al fine combatteo.
  Vedi Par�s, Tristano�; e pi� di mille
  ombre mostrommi e nominommi a dito,
  ch'amor di nostra vita dipartille.
  Poscia ch'io ebbi 'l mio dottore udito
  nomar le donne antiche e ' cavalieri,
  piet� mi giunse, e fui quasi smarrito.
  I' cominciai: �Poeta, volontieri
  parlerei a quei due che 'nsieme vanno,
  e paion s� al vento esser leggieri�.
  Ed elli a me: �Vedrai quando saranno
  pi� presso a noi; e tu allor li priega
  per quello amor che i mena, ed ei verranno�.
  S� tosto come il vento a noi li piega,
  mossi la voce: �O anime affannate,
  venite a noi parlar, s'altri nol niega!�.
  Quali colombe dal disio chiamate
  con l'ali alzate e ferme al dolce nido
  vegnon per l'aere, dal voler portate;
  cotali uscir de la schiera ov' � Dido,
  a noi venendo per l'aere maligno,
  s� forte fu l'affett�oso grido.
  �O animal graz�oso e benigno
  che visitando vai per l'aere perso
  noi che tignemmo il mondo di sanguigno,
  se fosse amico il re de l'universo,
  noi pregheremmo lui de la tua pace,
  poi c'hai piet� del nostro mal perverso.
  Di quel che udire e che parlar vi piace,
  noi udiremo e parleremo a voi,
  mentre che 'l vento, come fa, ci tace.
  Siede la terra dove nata fui
  su la marina dove 'l Po discende
  per aver pace co' seguaci sui.
  Amor, ch'al cor gentil ratto s'apprende,
  prese costui de la bella persona
  che mi fu tolta; e 'l modo ancor m'offende.
  Amor, ch'a nullo amato amar perdona,
  mi prese del costui piacer s� forte,
  che, come vedi, ancor non m'abbandona.
  Amor condusse noi ad una morte.
  Caina attende chi a vita ci spense�.
  Queste parole da lor ci fuor porte.
  Quand' io intesi quell' anime offense,
  china' il viso, e tanto il tenni basso,
  fin che 'l poeta mi disse: �Che pense?�.
  Quando rispuosi, cominciai: �Oh lasso,
  quanti dolci pensier, quanto disio
  men� costoro al doloroso passo!�.
  Poi mi rivolsi a loro e parla' io,
  e cominciai: �Francesca, i tuoi mart�ri
  a lagrimar mi fanno tristo e pio.
  Ma dimmi: al tempo d'i dolci sospiri,
  a che e come concedette amore
  che conosceste i dubbiosi disiri?�.
  E quella a me: �Nessun maggior dolore
  che ricordarsi del tempo felice
  ne la miseria; e ci� sa 'l tuo dottore.
  Ma s'a conoscer la prima radice
  del nostro amor tu hai cotanto affetto,
  dir� come colui che piange e dice.
  Noi leggiavamo un giorno per diletto
  di Lancialotto come amor lo strinse;
  soli eravamo e sanza alcun sospetto.
  Per pi� f�ate li occhi ci sospinse
  quella lettura, e scolorocci il viso;
  ma solo un punto fu quel che ci vinse.
  Quando leggemmo il dis�ato riso
  esser basciato da cotanto amante,
  questi, che mai da me non fia diviso,
  la bocca mi basci� tutto tremante.
  Galeotto fu 'l libro e chi lo scrisse:
  quel giorno pi� non vi leggemmo avante�.
  Mentre che l'uno spirto questo disse,
  l'altro piang�a; s� che di pietade
  io venni men cos� com' io morisse.
  E caddi come corpo morto cade.

  CANTO VI
  [Canto sesto, nel quale mostra del terzo cerchio de l'inferno e tratta del punimento del vizio de la gola, e massimamente in persona d'un fiorentino chiamato Ciacco; in confusione di tutt'i buffoni tratta del dimonio Cerbero e narra in forma di predicere pi� cose a divenire a la citt� di Fiorenza.]


  Al tornar de la mente, che si chiuse
  dinanzi a la piet� d'i due cognati,
  che di trestizia tutto mi confuse,
  novi tormenti e novi tormentati
  mi veggio intorno, come ch'io mi mova
  e ch'io mi volga, e come che io guati.
  Io sono al terzo cerchio, de la piova
  etterna, maladetta, fredda e greve;
  regola e qualit� mai non l'� nova.
  Grandine grossa, acqua tinta e neve
  per l'aere tenebroso si riversa;
  pute la terra che questo riceve.
  Cerbero, fiera crudele e diversa,
  con tre gole caninamente latra
  sovra la gente che quivi � sommersa.
  Li occhi ha vermigli, la barba unta e atra,
  e 'l ventre largo, e unghiate le mani;
  graffia li spirti ed iscoia ed isquatra.
  Urlar li fa la pioggia come cani;
  de l'un de' lati fanno a l'altro schermo;
  volgonsi spesso i miseri profani.
  Quando ci scorse Cerbero, il gran vermo,
  le bocche aperse e mostrocci le sanne;
  non avea membro che tenesse fermo.
  E 'l duca mio distese le sue spanne,
  prese la terra, e con piene le pugna
  la gitt� dentro a le bramose canne.
  Qual � quel cane ch'abbaiando agogna,
  e si racqueta poi che 'l pasto morde,
  ch� solo a divorarlo intende e pugna,
  cotai si fecer quelle facce lorde
  de lo demonio Cerbero, che 'ntrona
  l'anime s�, ch'esser vorrebber sorde.
  Noi passavam su per l'ombre che adona
  la greve pioggia, e ponavam le piante
  sovra lor vanit� che par persona.
  Elle giacean per terra tutte quante,
  fuor d'una ch'a seder si lev�, ratto
  ch'ella ci vide passarsi davante.
  �O tu che se' per questo 'nferno tratto�,
  mi disse, �riconoscimi, se sai:
  tu fosti, prima ch'io disfatto, fatto�.
  E io a lui: �L'angoscia che tu hai
  forse ti tira fuor de la mia mente,
  s� che non par ch'i' ti vedessi mai.
  Ma dimmi chi tu se' che 'n s� dolente
  loco se' messo, e hai s� fatta pena,
  che, s'altra � maggio, nulla � s� spiacente�.
  Ed elli a me: �La tua citt�, ch'� piena
  d'invidia s� che gi� trabocca il sacco,
  seco mi tenne in la vita serena.
  Voi cittadini mi chiamaste Ciacco:
  per la dannosa colpa de la gola,
  come tu vedi, a la pioggia mi fiacco.
  E io anima trista non son sola,
  ch� tutte queste a simil pena stanno
  per simil colpa�. E pi� non f� parola.
  Io li rispuosi: �Ciacco, il tuo affanno
  mi pesa s�, ch'a lagrimar mi 'nvita;
  ma dimmi, se tu sai, a che verranno
  li cittadin de la citt� partita;
  s'alcun v'� giusto; e dimmi la cagione
  per che l'ha tanta discordia assalita�.
  E quelli a me: �Dopo lunga tencione
  verranno al sangue, e la parte selvaggia
  caccer� l'altra con molta offensione.
  Poi appresso convien che questa caggia
  infra tre soli, e che l'altra sormonti
  con la forza di tal che test� piaggia.
  Alte terr� lungo tempo le fronti,
  tenendo l'altra sotto gravi pesi,
  come che di ci� pianga o che n'aonti.
  Giusti son due, e non vi sono intesi;
  superbia, invidia e avarizia sono
  le tre faville c'hanno i cuori accesi�.
  Qui puose fine al lagrimabil suono.
  E io a lui: �Ancor vo' che mi 'nsegni
  e che di pi� parlar mi facci dono.
  Farinata e 'l Tegghiaio, che fuor s� degni,
  Iacopo Rusticucci, Arrigo e 'l Mosca
  e li altri ch'a ben far puoser li 'ngegni,
  dimmi ove sono e fa ch'io li conosca;
  ch� gran disio mi stringe di savere
  se 'l ciel li addolcia o lo 'nferno li attosca�.
  E quelli: �Ei son tra l'anime pi� nere;
  diverse colpe gi� li grava al fondo:
  se tanto scendi, l� i potrai vedere.
  Ma quando tu sarai nel dolce mondo,
  priegoti ch'a la mente altrui mi rechi:
  pi� non ti dico e pi� non ti rispondo�.
  Li diritti occhi torse allora in biechi;
  guardommi un poco e poi chin� la testa:
  cadde con essa a par de li altri ciechi.
  E 'l duca disse a me: �Pi� non si desta
  di qua dal suon de l'angelica tromba,
  quando verr� la nimica podesta:
  ciascun riveder� la trista tomba,
  ripiglier� sua carne e sua figura,
  udir� quel ch'in etterno rimbomba�.
  S� trapassammo per sozza mistura
  de l'ombre e de la pioggia, a passi lenti,
  toccando un poco la vita futura;
  per ch'io dissi: �Maestro, esti tormenti
  crescerann' ei dopo la gran sentenza,
  o fier minori, o saran s� cocenti?�.
  Ed elli a me: �Ritorna a tua sc�enza,
  che vuol, quanto la cosa � pi� perfetta,
  pi� senta il bene, e cos� la doglienza.
  Tutto che questa gente maladetta
  in vera perfezion gi� mai non vada,
  di l� pi� che di qua essere aspetta�.
  Noi aggirammo a tondo quella strada,
  parlando pi� assai ch'i' non ridico;
  venimmo al punto dove si digrada:
  quivi trovammo Pluto, il gran nemico.

  CANTO VII
  [Canto settimo, dove si dimostra del quarto cerchio de l'inferno e alquanto del quinto; qui pone la pena del peccato de l'avarizia e del vizio de la prodigalit�; e del dimonio Pluto; e quello che � fortuna.]


  �Pape Sat�n, pape Sat�n aleppe!�,
  cominci� Pluto con la voce chioccia;
  e quel savio gentil, che tutto seppe,
  disse per confortarmi: �Non ti noccia
  la tua paura; ch�, poder ch'elli abbia,
  non ci torr� lo scender questa roccia�.
  Poi si rivolse a quella 'nfiata labbia,
  e disse: �Taci, maladetto lupo!
  consuma dentro te con la tua rabbia.
  Non � sanza cagion l'andare al cupo:
  vuolsi ne l'alto, l� dove Michele
  f� la vendetta del superbo strupo�.
  Quali dal vento le gonfiate vele
  caggiono avvolte, poi che l'alber fiacca,
  tal cadde a terra la fiera crudele.
  Cos� scendemmo ne la quarta lacca,
  pigliando pi� de la dolente ripa
  che 'l mal de l'universo tutto insacca.
  Ahi giustizia di Dio! tante chi stipa
  nove travaglie e pene quant' io viddi?
  e perch� nostra colpa s� ne scipa?
  Come fa l'onda l� sovra Cariddi,
  che si frange con quella in cui s'intoppa,
  cos� convien che qui la gente riddi.
  Qui vid' i' gente pi� ch'altrove troppa,
  e d'una parte e d'altra, con grand' urli,
  voltando pesi per forza di poppa.
  Percot�ansi 'ncontro; e poscia pur l�
  si rivolgea ciascun, voltando a retro,
  gridando: �Perch� tieni?� e �Perch� burli?�.
  Cos� tornavan per lo cerchio tetro
  da ogne mano a l'opposito punto,
  gridandosi anche loro ontoso metro;
  poi si volgea ciascun, quand' era giunto,
  per lo suo mezzo cerchio a l'altra giostra.
  E io, ch'avea lo cor quasi compunto,
  dissi: �Maestro mio, or mi dimostra
  che gente � questa, e se tutti fuor cherci
  questi chercuti a la sinistra nostra�.
  Ed elli a me: �Tutti quanti fuor guerci
  s� de la mente in la vita primaia,
  che con misura nullo spendio ferci.
  Assai la voce lor chiaro l'abbaia,
  quando vegnono a' due punti del cerchio
  dove colpa contraria li dispaia.
  Questi fuor cherci, che non han coperchio
  piloso al capo, e papi e cardinali,
  in cui usa avarizia il suo soperchio�.
  E io: �Maestro, tra questi cotali
  dovre' io ben riconoscere alcuni
  che furo immondi di cotesti mali�.
  Ed elli a me: �Vano pensiero aduni:
  la sconoscente vita che i f� sozzi,
  ad ogne conoscenza or li fa bruni.
  In etterno verranno a li due cozzi:
  questi resurgeranno del sepulcro
  col pugno chiuso, e questi coi crin mozzi.
  Mal dare e mal tener lo mondo pulcro
  ha tolto loro, e posti a questa zuffa:
  qual ella sia, parole non ci appulcro.
  Or puoi, figliuol, veder la corta buffa
  d'i ben che son commessi a la fortuna,
  per che l'umana gente si rabuffa;
  ch� tutto l'oro ch'� sotto la luna
  e che gi� fu, di quest' anime stanche
  non poterebbe farne posare una�.
  �Maestro mio�, diss' io, �or mi d� anche:
  questa fortuna di che tu mi tocche,
  che �, che i ben del mondo ha s� tra branche?�.
  E quelli a me: �Oh creature sciocche,
  quanta ignoranza � quella che v'offende!
  Or vo' che tu mia sentenza ne 'mbocche.
  Colui lo cui saver tutto trascende,
  fece li cieli e di� lor chi conduce
  s�, ch'ogne parte ad ogne parte splende,
  distribuendo igualmente la luce.
  Similemente a li splendor mondani
  ordin� general ministra e duce
  che permutasse a tempo li ben vani
  di gente in gente e d'uno in altro sangue,
  oltre la difension d'i senni umani;
  per ch'una gente impera e l'altra langue,
  seguendo lo giudicio di costei,
  che � occulto come in erba l'angue.
  Vostro saver non ha contasto a lei:
  questa provede, giudica, e persegue
  suo regno come il loro li altri d�i.
  Le sue permutazion non hanno triegue:
  necessit� la fa esser veloce;
  s� spesso vien chi vicenda consegue.
  Quest' � colei ch'� tanto posta in croce
  pur da color che le dovrien dar lode,
  dandole biasmo a torto e mala voce;
  ma ella s'� beata e ci� non ode:
  con l'altre prime creature lieta
  volve sua spera e beata si gode.
  Or discendiamo omai a maggior pieta;
  gi� ogne stella cade che saliva
  quand' io mi mossi, e 'l troppo star si vieta�.
  Noi ricidemmo il cerchio a l'altra riva
  sovr' una fonte che bolle e riversa
  per un fossato che da lei deriva.
  L'acqua era buia assai pi� che persa;
  e noi, in compagnia de l'onde bige,
  intrammo gi� per una via diversa.
  In la palude va c'ha nome Stige
  questo tristo ruscel, quand' � disceso
  al pi� de le maligne piagge grige.
  E io, che di mirare stava inteso,
  vidi genti fangose in quel pantano,
  ignude tutte, con sembiante offeso.
  Queste si percotean non pur con mano,
  ma con la testa e col petto e coi piedi,
  troncandosi co' denti a brano a brano.
  Lo buon maestro disse: �Figlio, or vedi
  l'anime di color cui vinse l'ira;
  e anche vo' che tu per certo credi
  che sotto l'acqua � gente che sospira,
  e fanno pullular quest' acqua al summo,
  come l'occhio ti dice, u' che s'aggira.
  Fitti nel limo dicon: "Tristi fummo
  ne l'aere dolce che dal sol s'allegra,
  portando dentro accid�oso fummo:
  or ci attristiam ne la belletta negra".
  Quest' inno si gorgoglian ne la strozza,
  ch� dir nol posson con parola integra�.
  Cos� girammo de la lorda pozza
  grand' arco, tra la ripa secca e 'l m�zzo,
  con li occhi v�lti a chi del fango ingozza.
  Venimmo al pi� d'una torre al da sezzo.

  CANTO VIII
  [Canto ottavo, ove tratta del quinto cerchio de l'inferno e alquanto del sesto, e de la pena del peccato de l'ira, massimamente in persona d'uno cavaliere fiorentino chiamato messer Filippo Argenti, e del dimonio Flegias e de la palude di Stige e del pervenire a la citt� d'inferno detta Dite.]


  Io dico, seguitando, ch'assai prima
  che noi fossimo al pi� de l'alta torre,
  li occhi nostri n'andar suso a la cima
  per due fiammette che i vedemmo porre,
  e un'altra da lungi render cenno,
  tanto ch'a pena il potea l'occhio t�rre.
  E io mi volsi al mar di tutto 'l senno;
  dissi: �Questo che dice? e che risponde
  quell' altro foco? e chi son quei che 'l fenno?�.
  Ed elli a me: �Su per le sucide onde
  gi� scorgere puoi quello che s'aspetta,
  se 'l fummo del pantan nol ti nasconde�.
  Corda non pinse mai da s� saetta
  che s� corresse via per l'aere snella,
  com' io vidi una nave piccioletta
  venir per l'acqua verso noi in quella,
  sotto 'l governo d'un sol galeoto,
  che gridava: �Or se' giunta, anima fella!�.
  �Fleg��s, Fleg��s, tu gridi a v�to�,
  disse lo mio segnore, �a questa volta:
  pi� non ci avrai che sol passando il loto�.
  Qual � colui che grande inganno ascolta
  che li sia fatto, e poi se ne rammarca,
  fecesi Fleg��s ne l'ira accolta.
  Lo duca mio discese ne la barca,
  e poi mi fece intrare appresso lui;
  e sol quand' io fui dentro parve carca.
  Tosto che 'l duca e io nel legno fui,
  segando se ne va l'antica prora
  de l'acqua pi� che non suol con altrui.
  Mentre noi corravam la morta gora,
  dinanzi mi si fece un pien di fango,
  e disse: �Chi se' tu che vieni anzi ora?�.
  E io a lui: �S'i' vegno, non rimango;
  ma tu chi se', che s� se' fatto brutto?�.
  Rispuose: �Vedi che son un che piango�.
  E io a lui: �Con piangere e con lutto,
  spirito maladetto, ti rimani;
  ch'i' ti conosco, ancor sie lordo tutto�.
  Allor distese al legno ambo le mani;
  per che 'l maestro accorto lo sospinse,
  dicendo: �Via cost� con li altri cani!�.
  Lo collo poi con le braccia mi cinse;
  basciommi 'l volto e disse: �Alma sdegnosa,
  benedetta colei che 'n te s'incinse!
  Quei fu al mondo persona orgogliosa;
  bont� non � che sua memoria fregi:
  cos� s'� l'ombra sua qui fur�osa.
  Quanti si tegnon or l� s� gran regi
  che qui staranno come porci in brago,
  di s� lasciando orribili dispregi!�.
  E io: �Maestro, molto sarei vago
  di vederlo attuffare in questa broda
  prima che noi uscissimo del lago�.
  Ed elli a me: �Avante che la proda
  ti si lasci veder, tu sarai sazio:
  di tal dis�o convien che tu goda�.
  Dopo ci� poco vid' io quello strazio
  far di costui a le fangose genti,
  che Dio ancor ne lodo e ne ringrazio.
  Tutti gridavano: �A Filippo Argenti!�;
  e 'l fiorentino spirito bizzarro
  in s� medesmo si volvea co' denti.
  Quivi il lasciammo, che pi� non ne narro;
  ma ne l'orecchie mi percosse un duolo,
  per ch'io avante l'occhio intento sbarro.
  Lo buon maestro disse: �Omai, figliuolo,
  s'appressa la citt� c'ha nome Dite,
  coi gravi cittadin, col grande stuolo�.
  E io: �Maestro, gi� le sue meschite
  l� entro certe ne la valle cerno,
  vermiglie come se di foco uscite
  fossero�. Ed ei mi disse: �Il foco etterno
  ch'entro l'affoca le dimostra rosse,
  come tu vedi in questo basso inferno�.
  Noi pur giugnemmo dentro a l'alte fosse
  che vallan quella terra sconsolata:
  le mura mi parean che ferro fosse.
  Non sanza prima far grande aggirata,
  venimmo in parte dove il nocchier forte
  �Usciteci�, grid�: �qui � l'intrata�.
  Io vidi pi� di mille in su le porte
  da ciel piovuti, che stizzosamente
  dicean: �Chi � costui che sanza morte
  va per lo regno de la morta gente?�.
  E 'l savio mio maestro fece segno
  di voler lor parlar segretamente.
  Allor chiusero un poco il gran disdegno
  e disser: �Vien tu solo, e quei sen vada
  che s� ardito intr� per questo regno.
  Sol si ritorni per la folle strada:
  pruovi, se sa; ch� tu qui rimarrai,
  che li ha' iscorta s� buia contrada�.
  Pensa, lettor, se io mi sconfortai
  nel suon de le parole maladette,
  ch� non credetti ritornarci mai.
  �O caro duca mio, che pi� di sette
  volte m'hai sicurt� renduta e tratto
  d'alto periglio che 'ncontra mi stette,
  non mi lasciar�, diss' io, �cos� disfatto;
  e se 'l passar pi� oltre ci � negato,
  ritroviam l'orme nostre insieme ratto�.
  E quel segnor che l� m'avea menato,
  mi disse: �Non temer; ch� 'l nostro passo
  non ci pu� t�rre alcun: da tal n'� dato.
  Ma qui m'attendi, e lo spirito lasso
  conforta e ciba di speranza buona,
  ch'i' non ti lascer� nel mondo basso�.
  Cos� sen va, e quivi m'abbandona
  lo dolce padre, e io rimagno in forse,
  che s� e no nel capo mi tenciona.
  Udir non potti quello ch'a lor porse;
  ma ei non stette l� con essi guari,
  che ciascun dentro a pruova si ricorse.
  Chiuser le porte que' nostri avversari
  nel petto al mio segnor, che fuor rimase
  e rivolsesi a me con passi rari.
  Li occhi a la terra e le ciglia avea rase
  d'ogne baldanza, e dicea ne' sospiri:
  �Chi m'ha negate le dolenti case!�.
  E a me disse: �Tu, perch' io m'adiri,
  non sbigottir, ch'io vincer� la prova,
  qual ch'a la difension dentro s'aggiri.
  Questa lor tracotanza non � nova;
  ch� gi� l'usaro a men segreta porta,
  la qual sanza serrame ancor si trova.
  Sovr' essa vedest� la scritta morta:
  e gi� di qua da lei discende l'erta,
  passando per li cerchi sanza scorta,
  tal che per lui ne fia la terra aperta�.

  CANTO IX
  [Canto nono, ove tratta e dimostra de la cittade c'ha nome Dite, la qual si � nel sesto cerchio de l'inferno e vedesi messa la qualit� de le pene de li eretici; e dichiara in questo canto Virgilio a Dante una questione, e rendelo sicuro dicendo s� esservi stato dentro altra fiata.]


  Quel color che vilt� di fuor mi pinse
  veggendo il duca mio tornare in volta,
  pi� tosto dentro il suo novo ristrinse.
  Attento si ferm� com' uom ch'ascolta;
  ch� l'occhio nol potea menare a lunga
  per l'aere nero e per la nebbia folta.
  �Pur a noi converr� vincer la punga�,
  cominci� el, �se non... Tal ne s'offerse.
  Oh quanto tarda a me ch'altri qui giunga!�.
  I' vidi ben s� com' ei ricoperse
  lo cominciar con l'altro che poi venne,
  che fur parole a le prime diverse;
  ma nondimen paura il suo dir dienne,
  perch' io traeva la parola tronca
  forse a peggior sentenzia che non tenne.
  �In questo fondo de la trista conca
  discende mai alcun del primo grado,
  che sol per pena ha la speranza cionca?�.
  Questa question fec' io; e quei �Di rado
  incontra�, mi rispuose, �che di noi
  faccia il cammino alcun per qual io vado.
  Ver � ch'altra f�ata qua gi� fui,
  congiurato da quella Erit�n cruda
  che richiamava l'ombre a' corpi sui.
  Di poco era di me la carne nuda,
  ch'ella mi fece intrar dentr' a quel muro,
  per trarne un spirto del cerchio di Giuda.
  Quell' � 'l pi� basso loco e 'l pi� oscuro,
  e 'l pi� lontan dal ciel che tutto gira:
  ben so 'l cammin; per� ti fa sicuro.
  Questa palude che 'l gran puzzo spira
  cigne dintorno la citt� dolente,
  u' non potemo intrare omai sanz' ira�.
  E altro disse, ma non l'ho a mente;
  per� che l'occhio m'avea tutto tratto
  ver' l'alta torre a la cima rovente,
  dove in un punto furon dritte ratto
  tre fur�e infernal di sangue tinte,
  che membra feminine avieno e atto,
  e con idre verdissime eran cinte;
  serpentelli e ceraste avien per crine,
  onde le fiere tempie erano avvinte.
  E quei, che ben conobbe le meschine
  de la regina de l'etterno pianto,
  �Guarda�, mi disse, �le feroci Erine.
  Quest' � Megera dal sinistro canto;
  quella che piange dal destro � Aletto;
  Tesif�n � nel mezzo�; e tacque a tanto.
  Con l'unghie si fendea ciascuna il petto;
  battiensi a palme e gridavan s� alto,
  ch'i' mi strinsi al poeta per sospetto.
  �Vegna Medusa: s� 'l farem di smalto�,
  dicevan tutte riguardando in giuso;
  �mal non vengiammo in Tes�o l'assalto�.
  �Volgiti 'n dietro e tien lo viso chiuso;
  ch� se 'l Gorg�n si mostra e tu 'l vedessi,
  nulla sarebbe di tornar mai suso�.
  Cos� disse 'l maestro; ed elli stessi
  mi volse, e non si tenne a le mie mani,
  che con le sue ancor non mi chiudessi.
  O voi ch'avete li 'ntelletti sani,
  mirate la dottrina che s'asconde
  sotto 'l velame de li versi strani.
  E gi� ven�a su per le torbide onde
  un fracasso d'un suon, pien di spavento,
  per cui tremavano amendue le sponde,
  non altrimenti fatto che d'un vento
  impet�oso per li avversi ardori,
  che fier la selva e sanz' alcun rattento
  li rami schianta, abbatte e porta fori;
  dinanzi polveroso va superbo,
  e fa fuggir le fiere e li pastori.
  Li occhi mi sciolse e disse: �Or drizza il nerbo
  del viso su per quella schiuma antica
  per indi ove quel fummo � pi� acerbo�.
  Come le rane innanzi a la nimica
  biscia per l'acqua si dileguan tutte,
  fin ch'a la terra ciascuna s'abbica,
  vid' io pi� di mille anime distrutte
  fuggir cos� dinanzi ad un ch'al passo
  passava Stige con le piante asciutte.
  Dal volto rimovea quell' aere grasso,
  menando la sinistra innanzi spesso;
  e sol di quell' angoscia parea lasso.
  Ben m'accorsi ch'elli era da ciel messo,
  e volsimi al maestro; e quei f� segno
  ch'i' stessi queto ed inchinassi ad esso.
  Ahi quanto mi parea pien di disdegno!
  Venne a la porta e con una verghetta
  l'aperse, che non v'ebbe alcun ritegno.
  �O cacciati del ciel, gente dispetta�,
  cominci� elli in su l'orribil soglia,
  �ond' esta oltracotanza in voi s'alletta?
  Perch� recalcitrate a quella voglia
  a cui non puote il fin mai esser mozzo,
  e che pi� volte v'ha cresciuta doglia?
  Che giova ne le fata dar di cozzo?
  Cerbero vostro, se ben vi ricorda,
  ne porta ancor pelato il mento e 'l gozzo�.
  Poi si rivolse per la strada lorda,
  e non f� motto a noi, ma f� sembiante
  d'omo cui altra cura stringa e morda
  che quella di colui che li � davante;
  e noi movemmo i piedi inver' la terra,
  sicuri appresso le parole sante.
  Dentro li 'ntrammo sanz' alcuna guerra;
  e io, ch'avea di riguardar disio
  la condizion che tal fortezza serra,
  com' io fui dentro, l'occhio intorno invio:
  e veggio ad ogne man grande campagna,
  piena di duolo e di tormento rio.
  S� come ad Arli, ove Rodano stagna,
  s� com' a Pola, presso del Carnaro
  ch'Italia chiude e suoi termini bagna,
  fanno i sepulcri tutt' il loco varo,
  cos� facevan quivi d'ogne parte,
  salvo che 'l modo v'era pi� amaro;
  ch� tra li avelli fiamme erano sparte,
  per le quali eran s� del tutto accesi,
  che ferro pi� non chiede verun' arte.
  Tutti li lor coperchi eran sospesi,
  e fuor n'uscivan s� duri lamenti,
  che ben parean di miseri e d'offesi.
  E io: �Maestro, quai son quelle genti
  che, seppellite dentro da quell' arche,
  si fan sentir coi sospiri dolenti?�.
  E quelli a me: �Qui son li eres�arche
  con lor seguaci, d'ogne setta, e molto
  pi� che non credi son le tombe carche.
  Simile qui con simile � sepolto,
  e i monimenti son pi� e men caldi�.
  E poi ch'a la man destra si fu v�lto,
  passammo tra i mart�ri e li alti spaldi.

  CANTO X
  [Canto decimo, ove tratta del sesto cerchio de l'inferno e de la pena de li eretici, e in forma d'indovinare in persona di messer Farinata predice molte cose e di quelle che avvennero a Dante, e solve una questione.]


  Ora sen va per un secreto calle,
  tra 'l muro de la terra e li mart�ri,
  lo mio maestro, e io dopo le spalle.
  �O virt� somma, che per li empi giri
  mi volvi�, cominciai, �com' a te piace,
  parlami, e sodisfammi a' miei disiri.
  La gente che per li sepolcri giace
  potrebbesi veder? gi� son levati
  tutt' i coperchi, e nessun guardia face�.
  E quelli a me: �Tutti saran serrati
  quando di Iosaf�t qui torneranno
  coi corpi che l� s� hanno lasciati.
  Suo cimitero da questa parte hanno
  con Epicuro tutti suoi seguaci,
  che l'anima col corpo morta fanno.
  Per� a la dimanda che mi faci
  quinc' entro satisfatto sar� tosto,
  e al disio ancor che tu mi taci�.
  E io: �Buon duca, non tegno riposto
  a te mio cuor se non per dicer poco,
  e tu m'hai non pur mo a ci� disposto�.
  �O Tosco che per la citt� del foco
  vivo ten vai cos� parlando onesto,
  piacciati di restare in questo loco.
  La tua loquela ti fa manifesto
  di quella nobil patr�a natio,
  a la qual forse fui troppo molesto�.
  Subitamente questo suono usc�o
  d'una de l'arche; per� m'accostai,
  temendo, un poco pi� al duca mio.
  Ed el mi disse: �Volgiti! Che fai?
  Vedi l� Farinata che s'� dritto:
  da la cintola in s� tutto 'l vedrai�.
  Io avea gi� il mio viso nel suo fitto;
  ed el s'ergea col petto e con la fronte
  com' avesse l'inferno a gran dispitto.
  E l'animose man del duca e pronte
  mi pinser tra le sepulture a lui,
  dicendo: �Le parole tue sien conte�.
  Com' io al pi� de la sua tomba fui,
  guardommi un poco, e poi, quasi sdegnoso,
  mi dimand�: �Chi fuor li maggior tui?�.
  Io ch'era d'ubidir disideroso,
  non gliel celai, ma tutto gliel' apersi;
  ond' ei lev� le ciglia un poco in suso;
  poi disse: �Fieramente furo avversi
  a me e a miei primi e a mia parte,
  s� che per due f�ate li dispersi�.
  �S'ei fur cacciati, ei tornar d'ogne parte�,
  rispuos' io lui, �l'una e l'altra f�ata;
  ma i vostri non appreser ben quell' arte�.
  Allor surse a la vista scoperchiata
  un'ombra, lungo questa, infino al mento:
  credo che s'era in ginocchie levata.
  Dintorno mi guard�, come talento
  avesse di veder s'altri era meco;
  e poi che 'l sospecciar fu tutto spento,
  piangendo disse: �Se per questo cieco
  carcere vai per altezza d'ingegno,
  mio figlio ov' �? e perch� non � teco?�.
  E io a lui: �Da me stesso non vegno:
  colui ch'attende l�, per qui mi mena
  forse cui Guido vostro ebbe a disdegno�.
  Le sue parole e 'l modo de la pena
  m'avean di costui gi� letto il nome;
  per� fu la risposta cos� piena.
  Di s�bito drizzato grid�: �Come?
  dicesti "elli ebbe"? non viv' elli ancora?
  non fiere li occhi suoi lo dolce lume?�.
  Quando s'accorse d'alcuna dimora
  ch'io fac�a dinanzi a la risposta,
  supin ricadde e pi� non parve fora.
  Ma quell' altro magnanimo, a cui posta
  restato m'era, non mut� aspetto,
  n� mosse collo, n� pieg� sua costa;
  e s� contin�ando al primo detto,
  �S'elli han quell' arte�, disse, �male appresa,
  ci� mi tormenta pi� che questo letto.
  Ma non cinquanta volte fia raccesa
  la faccia de la donna che qui regge,
  che tu saprai quanto quell' arte pesa.
  E se tu mai nel dolce mondo regge,
  dimmi: perch� quel popolo � s� empio
  incontr' a' miei in ciascuna sua legge?�.
  Ond' io a lui: �Lo strazio e 'l grande scempio
  che fece l'Arbia colorata in rosso,
  tal orazion fa far nel nostro tempio�.
  Poi ch'ebbe sospirando il capo mosso,
  �A ci� non fu' io sol�, disse, �n� certo
  sanza cagion con li altri sarei mosso.
  Ma fu' io solo, l� dove sofferto
  fu per ciascun di t�rre via Fiorenza,
  colui che la difesi a viso aperto�.
  �Deh, se riposi mai vostra semenza�,
  prega' io lui, �solvetemi quel nodo
  che qui ha 'nviluppata mia sentenza.
  El par che voi veggiate, se ben odo,
  dinanzi quel che 'l tempo seco adduce,
  e nel presente tenete altro modo�.
  �Noi veggiam, come quei c'ha mala luce,
  le cose�, disse, �che ne son lontano;
  cotanto ancor ne splende il sommo duce.
  Quando s'appressano o son, tutto � vano
  nostro intelletto; e s'altri non ci apporta,
  nulla sapem di vostro stato umano.
  Per� comprender puoi che tutta morta
  fia nostra conoscenza da quel punto
  che del futuro fia chiusa la porta�.
  Allor, come di mia colpa compunto,
  dissi: �Or direte dunque a quel caduto
  che 'l suo nato � co' vivi ancor congiunto;
  e s'i' fui, dianzi, a la risposta muto,
  fate i saper che 'l fei perch� pensava
  gi� ne l'error che m'avete soluto�.
  E gi� 'l maestro mio mi richiamava;
  per ch'i' pregai lo spirto pi� avaccio
  che mi dicesse chi con lu' istava.
  Dissemi: �Qui con pi� di mille giaccio:
  qua dentro � 'l secondo Federico
  e 'l Cardinale; e de li altri mi taccio�.
  Indi s'ascose; e io inver' l'antico
  poeta volsi i passi, ripensando
  a quel parlar che mi parea nemico.
  Elli si mosse; e poi, cos� andando,
  mi disse: �Perch� se' tu s� smarrito?�.
  E io li sodisfeci al suo dimando.
  �La mente tua conservi quel ch'udito
  hai contra te�, mi comand� quel saggio;
  �e ora attendi qui�, e drizz� 'l dito:
  �quando sarai dinanzi al dolce raggio
  di quella il cui bell' occhio tutto vede,
  da lei saprai di tua vita il v�aggio�.
  Appresso mosse a man sinistra il piede:
  lasciammo il muro e gimmo inver' lo mezzo
  per un sentier ch'a una valle fiede,
  che 'nfin l� s� facea spiacer suo lezzo.
  """


}
