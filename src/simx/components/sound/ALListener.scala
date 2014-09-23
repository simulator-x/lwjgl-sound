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
import simx.core.ontology.types
import org.lwjgl.BufferUtils
import org.lwjgl.openal.AL10._
import simx.core.entity.Entity
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.svaractor.unifiedaccess.{EntityUpdateHandling, ObservableAccessSet}


/**
 * User: dwiebusch
 * Date: 21.11.12
 * Time: 15:07
 */
object ALListener{
  private var posAccess : Option[ObservableAccessSet[types.Transformation.dataType, types.Transformation.dataType]] = None
  private var position = Vec3.Zero

  def connect(entity : Entity, sVar : ConvertibleTrait[types.Transformation.dataType])(implicit actorContext : EntityUpdateHandling){
    val access = entity.observe(sVar)
//    posAccess.collect{ case x => x.ignore() }
    posAccess = Some(access)
    entity.observe(sVar).head( pos => setPosition(pos))
    entity.get(sVar).head( pos => setPosition(pos))
  }

  def setPosition(pos : ConstMat4){

    setPosition(pos(3).xyz)
    //setOrientation((pos * Vec4(0,0,-1, 0)).xyz, (pos * Vec4(0,1,0,0)).xyz)
  }


  def setPosition(pos : ConstVec3){
//    println("Setting ALListener Position: " + pos )
    alListener3f(AL_POSITION, pos.x, pos.y, pos.z)
    position = pos
  }


  def setOrientation(at : ConstVec3, up : ConstVec3){
    //println("Setting ALListener Orientation, AT:  " + at + ", UP: " + up )
    val buf = BufferUtils.createFloatBuffer(6).put(Array[Float](at.x, at.y, at.z, up.x, up.y, up.z))
    buf.rewind()
    alListener(AL_ORIENTATION, buf)
  }

  // Setzt allg. Lautstaerke (welche beim Hoerer angekommt )
  def setListenerGain(gain : Float) {
    alListenerf(AL_GAIN, gain)
  }
}

