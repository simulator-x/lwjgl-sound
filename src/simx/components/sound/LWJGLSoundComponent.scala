/*
 * Copyright 2012 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.sound

import simplex3d.math.float._
import simx.core.helper.Execute
import simx.core.ontology.{types, GroundedSymbol, Symbols}
import simx.core.worldinterface.eventhandling.{Event, EventDescription, EventHandler}
import simx.core.entity.description.{NamedSValSet, EntityAspect, SValSet}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.component.EntityConfigLayer
import simx.core.entity.Entity
import java.io.File
import simx.core.ontology.types.SoundProperties

/**
 * Created with IntelliJ IDEA.
 * User: dwiebusch
 */

case class LWJGLSoundComponentAspect(name : Symbol) extends SoundComponentAspect[LWJGLSoundComponent](name){
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType)
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set()
}

case class EntityMatcher(e : Entity) extends Serializable{
  val matchEvent : PartialFunction[Event, Boolean] = {
    case ev : Event => ev.affectedEntities.contains(e)
  }
}

case class MaterialMatcher() extends Serializable{
  val matchEvent : PartialFunction[Event, Boolean] = {
    case event => event.affectedEntities.forall(_.containsSVars(types.Material))
  }
}



class LWJGLSoundComponent(name : Symbol) extends SoundComponent(name) with EntityConfigLayer with EventHandler{
  private type AudioData           = String
  protected var knownEntities      = Set[Entity]()
  protected var positions          = Map[Entity, ConstVec3]()
  protected var listenerPosition   = ConstVec3
  protected var stopSounds         = false
  protected var audioFiles         = Map[Entity, types.AudioFile.dataType]()
  protected var gains              = Map[Entity, Float]()
  protected var pitches            = Map[Entity, Float]()
  protected var loops              = Map[Entity, Boolean]()
  protected var maxDistances       = Map[Entity, Float]()
  protected var refDistances       = Map[Entity, Float]()
  protected var sounds             = Map[types.AudioFile.dataType, AudioData]()
  protected var eventMapping       = Map[Entity, Map[EventDescription, types.AudioFile.dataType]]()
  protected var combinationMapping = Map[GroundedSymbol, List[(Entity, SValSet, types.AudioFile.dataType)]]()
  protected var soundProps         = Map[Entity, Array[Any]]()
  protected var playingMap         = Map[Entity, List[ALSource]]()


  //override protected implicit val actorContext = this

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    SValSet()

  protected def finalizeConfiguration(e: Entity){}

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) {
    //println("[Setting Listener Pos]")
    params.getFirstValueFor(types.User).collect{ case user => ALListener.connect(user, types.ViewPlatform) }
  }

  override protected def startUp() {
    //println("[Start Up SoundComp]")
    ALListener.setPosition(Vec3.Zero)
    ALListener.setOrientation(Vec3(0,0,-1), Vec3(0,1,0))
    SoundEvents.sound.observe{ e =>
      if (e.get(types.AudioFile).isDefined && e.affectedEntities.nonEmpty){
        val entity = e.affectedEntities.head
        playFor(entity, e.get(types.AudioFile).get, e.get(types.Transformation).getOrElse(Mat4.Identity)(3).xyz, 1)
      } else
        playAll(e)
    }
  }

  private def playSoundAtWith(e : Entity, s : types.AudioFile.dataType, pos : ConstVec3, andThen : => Any = {},
                              gain : Float, loop : Boolean, pitch : Float, maxDist : Float, refDist : Float){
    sounds.get(s).collect{ case file =>
      SourceManager.getSource(new File(file)) match {
        case Some(sound) =>
          //println("PLAY " + s + " WITH POS: " + pos)
          sound.setPosition(pos)
          sound.setGain(gain)
          sound.setLoop(loop)
          sound.setPitch(pitch)
          sound.setFadingDistance(refDist, maxDist)
          sound.play()
          playingMap = playingMap.updated(e, sound :: playingMap.getOrElse(e, Nil))
          waitForEnd(e, sound, andThen)
        case None => // Error
      }
    }
  }

  private def waitForEnd(e : Entity, source :  ALSource, andThen : => Any){
    if (source.isPlaying) addJobIn(50){ waitForEnd(e, source, andThen) } else {
      playingMap = playingMap.updated(e, playingMap.getOrElse(e, Nil).filterNot(_.equals(source)))
      andThen
    }
  }

  protected def loadSound(fName : String){
    sounds = sounds + (fName -> fName)
  }

  protected def stopSoundsFor( e : Entity ){
    playingMap.getOrElse(e, Nil).foreach(_.stop())
    playingMap = playingMap.filterNot(_._1 == e)
  }

  //  private def playSound(s : types.AudioFile.dataType, andThen : => Any = {}){
  //    playSoundAt(s, Vec3.Zero, andThen)
  //  }

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e: Entity) {
    knownEntities = knownEntities - e
    if (dontStopOnRemove.contains(e))
      dontStopOnRemove -= e
    else
      stopSoundsFor(e)
  }

  private def updateFor(e: Entity, sp : SoundProperties){
    gains = gains.updated(e, sp.gain)
    loops = loops.updated(e, sp.loop)
    pitches = pitches.updated(e, sp.pitch)
    maxDistances = maxDistances.updated(e, sp.max_distance)
    refDistances = refDistances.updated(e, sp.ref_distance)
  }

  /**
   * calls provideInitialValues with the full set of initial values
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity, given: SValSet) {
    val cParams = aspect.getCreateParams
    def defaultTask(){
      provideInitialValues(e, cParams.combineWithValues(toProvide)._1)
      cParams.getAllValuesFor(types.AudioFile).foreach(loadSound)
    }

    cParams.semantics match {
      case Symbols.component     => defaultTask()
      case Symbols.audioFile     =>  // simply load a file and connect it with the entity
        cParams.getFirstValueFor(types.OpenAlProps).collect{
          case sp : SoundProperties => updateFor(e, sp)
        }
        cParams.getFirstValueFor(types.Position).collect{
          case pos : ConstVec3 =>
            positions = positions.updated( e, pos )
        }
        defaultTask()
      case Symbols.material => // associate an material with the entity
        defaultTask()
      case Symbols.openAlProps =>
        cParams.getFirstValueFor(types.OpenAlProps).collect{
          case sp : SoundProperties => updateFor(e, sp)
        }
        defaultTask()
      case Symbols.enabled =>
        cParams.getFirstValueFor(types.Enabled).collect{
          case stop : Boolean =>
            if(stop){
              SourceManager.stopSources()
            }
        }
        defaultTask()
      case Symbols.viewPlatform =>    // associate ViewPos with the entity
        defaultTask()
      case Symbols.event    =>  // like explode etc.
        cParams.getFirstValueFor(types.EventDescription).collect{
          case eDesc => cParams.getFirstValueFor(types.AudioFile).collect{
            case sound =>
              eDesc.restrictedBy(EntityMatcher(e).matchEvent).observe(handleSoundEvent)
              eventMapping = eventMapping.updated(e,
                eventMapping.getOrElse(e, Map[EventDescription, types.AudioFile.dataType]()).updated(eDesc, sound))
          }
        }
        cParams.getFirstValueFor(types.OpenAlProps).collect{
          case sp : SoundProperties => updateFor(e, sp)
        }
        defaultTask()
      case Symbols.eventDescription => // material based sound like slide, scratch, collide
        cParams.getFirstValueFor(types.EventDescription).collect{
          case eDesc => cParams.getFirstValueFor(types.Container).collect{
            case params => cParams.getFirstValueFor(types.AudioFile).collect{
              case file =>
                eDesc.restrictedBy(MaterialMatcher().matchEvent).observe(handleSoundEvent)
                val list = (e, params, file) :: combinationMapping.getOrElse(eDesc.name, Nil)
                combinationMapping = combinationMapping + ( eDesc.name -> list)
            }
          }
        }
        cParams.getFirstValueFor(types.OpenAlProps).collect{
          case sp : SoundProperties => updateFor(e, sp)
        }
        defaultTask()
      case sem => println("sound component: unknown semantics in requestInitialValues " + sem.value)
    }
  }

  /**
   * used to integrate the entity into the local representation
   * @param e the entity to be integrated
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    val cParams = aspect.getCreateParams
    knownEntities = knownEntities + e
    cParams.semantics match {
      case Symbols.eventDescription =>
      case Symbols.event =>
      case Symbols.material =>
      case Symbols.openAlProps =>
      case Symbols.enabled =>
      case Symbols.component =>
      case Symbols.viewPlatform =>
        e.observe(types.ViewPlatform).head { trafo =>
          ALListener.setPosition(trafo(3).xyz)
          //TODO: check new simplex values
          ALListener.setOrientation(Vec3(-trafo.m20, -trafo.m21, -trafo.m22), Vec3(trafo.m10, trafo.m11, trafo.m12))
        }
        e.get(types.ViewPlatform).head { trafo =>
          ALListener.setPosition(trafo(3).xyz)
          //TODO: check new simplex values
          ALListener.setOrientation(Vec3(-trafo.m20, -trafo.m21, -trafo.m22), Vec3(trafo.m10, trafo.m11, trafo.m12))
        }
      case Symbols.audioFile =>
        if (aspect.getCreateParams.getFirstValueFor(types.Threshold).getOrElse(-1f) > 0)
          dontStopOnRemove += e

        e.observe(types.OpenAlProps).head { updateFor(e, _) }
        e.observe(types.Position).head{ pos => positions = positions.updated(e, pos) }

        e.observe(types.AudioFile).head{ file => audioFiles = audioFiles.updated(e, file ) }
        e.get(types.AudioFile).head{ file => audioFiles = audioFiles.updated(e, file ) }

        e.observe(types.Enabled).head( value => playIt(e)(value) )
        e.get(types.Enabled).head{ value =>
          if (value || aspect.getCreateParams.getFirstValueFor(types.Enabled).getOrElse(false))
            playIt(e)(really = true)
        }
      case sem =>
        println("sound component: unknown semantics in entityConfigComplete " + sem.value)
    }
  }

  private var dontStopOnRemove = Set[Entity]()

  private def playIt(e : Entity)(really : Boolean){
    if (really) audioFiles.get(e) match {
      case Some(file) =>
        playFor(e, file, positions.getOrElse(e, Vec3.Zero), 1f)
      case None =>
    } else
      stopSoundsFor(e)
  }

  private def playAtCenter(e : Event, entity : Entity, sound : types.AudioFile.dataType, speedFactor : Float) {
    val tasks = createTasks(e, types.Transformation)

    if (tasks.isEmpty)
      playFor(entity, sound, Vec3.Zero, speedFactor)
    else
      Execute allSerialized2 tasks exec {
        results =>
          val count  = results.length.toFloat
          val center = results.foldLeft(Vec3.Zero){ (a,b)=> a + b(3).xyz } * (1f/count)
          playFor(entity, sound, center, speedFactor)
      }
  }

  private def playFor(entity : Entity, sound : types.AudioFile.dataType, center : ConstVec3, speedFactor : Float){
    playSoundAtWith(entity, sound, center, entity.getSVars(types.Enabled).headOption.collect{ case s => s._2.set(false) },
      gains.getOrElse(entity, 0.5F) * speedFactor,
      loops.getOrElse(entity, false), pitches.getOrElse(entity, 1.0F),
      maxDistances.getOrElse(entity, Float.MaxValue), refDistances.getOrElse(entity, 1.0F))
  }

  private def playAll(e : Event) {
    val entity = e.affectedEntities.headOption.orNull
    val fileTasks = e.affectedEntities.map(_.getSVars(types.AudioFile)).map{
      t => if (t.nonEmpty) t.head._2.get(_ : String => Unit)  else (handler : String  => Any ) => handler("") : Unit
    }
    if (fileTasks.isEmpty) return
    val posTasks  = e.affectedEntities.map(_.getSVars(types.Transformation)).map{
      t => if (t.nonEmpty) t.head._2.get(_ : ConstMat4 => Unit) else (handler : ConstMat4  => Any ) => handler(Mat4.Identity) : Unit
    }
    Execute allSerialized2 fileTasks.toSeq exec {
      files => Execute allSerialized2 posTasks.toSeq exec {
        positions => files.zip(positions).foreach {
          tuple => if (!tuple._1.isEmpty) playFor (entity, tuple._1, tuple._2(3).xyz, 1)
        }
      }
    }
  }

  private def findCombination(mappings : List[(Entity, SValSet, String)], toMatch : List[types.Material.dataType]) =
    mappings.find{
      mapping =>
        val list = mapping._2.toSValSeq
        toMatch.forall(list.map(_.value).contains) && list.map(_.value).forall(toMatch.contains)
    }.collect{ case triple => triple._1 -> triple._3 }


  private def createTasks[T](e : Event, toGet : ConvertibleTrait[T] ) =
    e.affectedEntities.map(_.getSVars(toGet)).filter(_.nonEmpty).map( x => x.head._2.get(_ : T => Unit)).toSeq


  private var recentEvents = List[(Long, Event)]()
  private val filterTime = 50L

  private def handleSoundEvent(e: Event) {
    // some filtering
    val now = System.currentTimeMillis()
    recentEvents = recentEvents.filter(_._1 > now - filterTime)
    if (recentEvents.exists{that => that._2.name.equals(e.name) && that._2.affectedEntities.equals(e.affectedEntities)})
      return
    recentEvents = (e.get(types.Time).getOrElse(now) -> e) :: recentEvents

    // eventbased sound rendering (single entity)
    if (e.affectedEntities.size == 1)
      eventMapping.get(e.affectedEntities.head).collect {
        case m => m.find( _._1 matches e ) match {
          case Some((_, soundFile)) =>
            playAtCenter(e, e.affectedEntities.head, soundFile, 1f)
          case None =>
          // error
        }
      }
    // combined rendering
    else {
      val mappings = combinationMapping.getOrElse(e.name, Nil)
      val tasks = createTasks(e, types.Material)
      var speedVal = 1f
      e.get(types.Factor).collect {
        case speedFactor : Float =>
          speedVal = speedFactor
      }
      if (tasks.nonEmpty) Execute allSerialized2 tasks exec {
        results =>
          //println(results)
          findCombination(mappings, results).collect{
            case (entity, sound) =>
              //println(sound)
              e.get(types.Transformation) match {
                case Some(pos) => playFor(entity, sound, pos(3).xyz, speedVal)
                case None      => playAtCenter( e, entity, sound, speedVal )
              }
          }
      }
    }
  }


  protected def performSimulationStep() {
    simulationCompleted()
  }
}
