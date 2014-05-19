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

import java.net.URL
import java.io.{FileNotFoundException, File}
import org.lwjgl.util.WaveData
import org.lwjgl.openal.AL10._
import org.lwjgl.openal.AL

/**
 * User: dwiebusch
 * Date: 21.11.12
 * Time: 17:23
 */
protected object BufferManager {
  protected var buffers = Map[URL, ALBuffer]()

  if (!AL.isCreated)
      AL.create()

  def getBuffer(file : File) = {
    if (!file.exists())
      throw new FileNotFoundException(file.getAbsolutePath)
    val url = file.toURI.toURL
    buffers.get(url).getOrElse(createBuffer(url))
  }

  private def createBuffer(url : URL) = {
    val wavefile = WaveData.create(url)
    val buffer = ALBuffer(alGenBuffers())
    buffers = buffers.updated(url, buffer)
    alBufferData(buffer.id, wavefile.format, wavefile.data, wavefile.samplerate)
    wavefile.dispose()
    buffer
  }

}

protected case class ALBuffer(id : Int)