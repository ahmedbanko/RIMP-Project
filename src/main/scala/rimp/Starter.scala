package rimp

import javafx.application.Application
import javafx.scene.Scene
import javafx.stage.Stage

class Starter extends Application {
  override def start(primaryStage: Stage): Unit = {
    val i = new Interpreter
    val exampleProg1 =
      """fact := 1;
    n := 3;
    while (!n > 0) do {
        fact := !n * !fact;
        n := !n - 1}
        """
    val p = i.parse(exampleProg1)

    val root = PaneBuilder.createPane("RIMP_InterpreterGUI.fxml");
    val scene = new Scene(root)
    primaryStage.setTitle("RIMP Interpreter")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}