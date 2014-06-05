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


import simx.core.entity.description._
import simx.core.ontology.{GroundedSymbol, Symbols}
import simx.core.ontology.types
import simx.core.entity.description.NamedSValSet
import simx.core.worldinterface.eventhandling.EventDescription
import simplex3d.math.float.{ConstVec3, ConstMat4}

/**
 * User: dwiebusch
 * Date: 06.10.12
 */

abstract class SoundAspect( aspectType : GroundedSymbol, targets : List[Symbol] = Nil )
  extends EntityAspect(Symbols.sound, aspectType, targets)

// for plain audio entities or to add a specific sound to an entity
case class SoundFile(fileName : String, soundprops : types.SoundProperties, pos : ConstVec3, playOnCreation : Boolean = false)
  extends SoundAspect(Symbols.audioFile)
{
  def getFeatures =
    Set(types.AudioFile, types.Enabled, types.OpenAlProps, types.Position)

  def getProvidings =
    getFeatures

  def getCreateParams =
    NamedSValSet(aspectType, types.AudioFile(fileName), types.Enabled(playOnCreation), types.OpenAlProps(soundprops), types.Position(pos))
}

// add an sound to the entity for events like explosions
case class EventSound(fileName : String, eventDesc : EventDescription, soundprops : types.SoundProperties)
  extends SoundAspect(Symbols.event)
{
  def getFeatures =
    Set(types.AudioFile, types.EventDescription, types.OpenAlProps)

  def getProvidings =
    getFeatures

  def getCreateParams =
    addCVars(types.AudioFile(fileName) and types.EventDescription(eventDesc) and types.OpenAlProps(soundprops))
}

// parameters are e.g. materials which are involved in an collision making this sound
case class MaterialBasedSound(fileName : String, eventDesc : EventDescription, parameters : SValSet = SValSet(), soundprops : types.SoundProperties)  // like slide, scratch, collide
  extends SoundAspect(Symbols.eventDescription)
{
  def getFeatures =
    Set(types.AudioFile, types.EventDescription, types.Container, types.OpenAlProps)

  def getProvidings =
    getFeatures

  def getCreateParams =
    addCVars(types.Container(parameters) and types.AudioFile(fileName) and types.EventDescription(eventDesc) and types.OpenAlProps(soundprops))
}


// material associated with an entity
case class SoundMaterial(material : types.Material.dataType)
  extends SoundAspect(Symbols.material)
{
  def getFeatures =
    Set(types.Material)

  def getProvidings =
    getFeatures

  def getCreateParams =
    new NamedSValSet(aspectType, types.Material(material))
}

case class SoundProps(soundprops : types.SoundProperties)
  extends SoundAspect(Symbols.openAlProps)
{
  def getFeatures =
    Set(types.OpenAlProps)

  def getProvidings =
    getFeatures

  def getCreateParams =
    new NamedSValSet(aspectType, types.OpenAlProps(soundprops))
}

case class StopSound(stop : Boolean)
  extends SoundAspect(Symbols.enabled)
{
  def getFeatures =
    Set(types.Enabled)

  def getProvidings =
    getFeatures

  def getCreateParams =
    new NamedSValSet(aspectType, types.Enabled(stop))
}


case class ListenerPosition(pos: ConstMat4)
  extends SoundAspect(Symbols.viewPlatform)
{
  def getFeatures =
    Set(types.ViewPlatform)

  def getProvidings =
    getFeatures

  def getCreateParams =
    new NamedSValSet(aspectType, types.ViewPlatform(pos))

}



