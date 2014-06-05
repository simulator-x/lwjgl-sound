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

import java.io.File
import org.lwjgl.BufferUtils
import org.lwjgl.openal.AL10
import org.lwjgl.openal.AL10._
import simplex3d.math.float.{Mat4, Vec3, ConstVec3}

/**
 * User: dwiebusch
 * Date: 21.11.12
 * Time: 16:17
 */
protected object SourceManager{
  protected var freeSources = List[ALSource]()
  protected var usedSources = List[ALSource]()
  protected var maxSources = 1024

  def init(){
    var actualSources = 1
    while (actualSources <= maxSources){
      val sources = createSources(actualSources)
      if (alGetError() == AL_NO_ERROR){
        alDeleteSources(sources)
      } else
        maxSources = actualSources-1
      actualSources += 1
    }
    val sources = createSources(maxSources)
    sources.rewind()
    while(sources.hasRemaining)
      freeSources = ALSource(sources.get()) :: freeSources
  }

  private def createSources(num : Int) = {
    val sources = BufferUtils.createIntBuffer(num)
    alGenSources(sources)
    sources
  }

  protected def updateSources(){
    freeSources = freeSources ::: usedSources.filterNot(_.isPlaying)
    usedSources = usedSources.filter(_.isPlaying)
  }

  protected def getFreeSource : Option[ALSource] = {
    if (freeSources.isEmpty)
      updateSources()
    freeSources match {
      case head :: tail =>
        usedSources = head :: usedSources
        freeSources = tail
        Some(head)
      case Nil =>
        println("no alsource available")
        None
    }
  }

  def getSource(file : File) = getFreeSource.collect{
    case s => s.setBuffer(BufferManager.getBuffer(file))
  }

  def stopSources() {
    usedSources.foreach((source) => source.stop())
  }



  def main(args : Array[String]){
    init()
    ALListener.setPosition(Mat4.Identity)
    getSource(new File("sounds/ghost.wav")) match {
      case Some(source) => source.setPosition(Vec3(2, 0, 0)).play()
      case None => println("something went wrong...")
    }
    Thread.sleep(2000)
  }
}

protected case class ALSource(id : Int){
  def isPlaying =
    alGetSourcei(id, AL_SOURCE_STATE) == AL_PLAYING

  def setBuffer( buffer : ALBuffer ) =  {
    alSourcei( id, AL_BUFFER, buffer.id )
    this
  }

  def setPosition(pos : ConstVec3) = {
//    println("Setting ALSource Position: " + pos )
    alSource3f(id, AL_POSITION, pos.x, pos.y, pos.z)
    this
  }

  def play(){
    alSourceRewind(id)
    alSourcePlay(id)
  }

  def stop() {
    alSourceStop(id)
  }

  def setLoop(loop : Boolean) {
//    println("SETTING LOOP: " + loop)
    if (loop) {
      alSourcei(id, AL_LOOPING, AL10.AL_TRUE)
    } else {
      alSourcei(id, AL_LOOPING, AL10.AL_FALSE)
    }

  }

  def setPitch(pitch : Float) {
    alSourcef(id, AL_PITCH, pitch)
  }

  // Setzt Lautstaerke einer bestimmten Soundquelle/Source
  def setGain(gain : Float) {
    alSourcef(id, AL_GAIN, gain)
  }

  def setFadingDistance(ref : Float, max : Float) {
    alSourcef(id, AL_REFERENCE_DISTANCE, ref)
    alSourcef(id, AL_MAX_DISTANCE, max)
  }
}