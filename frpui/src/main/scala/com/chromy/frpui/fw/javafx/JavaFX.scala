package com.chromy.frpui.fw.javafx

import javafx.fxml.FXMLLoader
import javafx.scene.Parent

import com.chromy.frpui.RendererChain.RendererChain
import com.chromy.frpui.fw.core.{RendererContext, SideEffect}

import scala.util.Try

/**
 * Created by cry on 2015.11.01..
 */
object JavaFX {
  def apply[M <: BaseController : Manifest](context: RendererContext, render: RendererChain[M#C], initialState: M#C): Try[(Parent, M, SideEffect)] = {
    val ct = manifest[M]

    Try {
      val clazzName = if (ct.runtimeClass.getSimpleName.endsWith("$")) ct.runtimeClass.getSimpleName.dropRight(1) else ct.runtimeClass.getSimpleName

      val loader = new FXMLLoader(ct.runtimeClass.getResource(s"$clazzName.fxml"))

      val node: Parent = loader.load()
      val controller = loader.getController[M]

      val sideEffect = controller.init(context, render.asInstanceOf[RendererChain[controller.C]], initialState.asInstanceOf[controller.C])
      
      (node, controller, sideEffect)
    }

  }
}
