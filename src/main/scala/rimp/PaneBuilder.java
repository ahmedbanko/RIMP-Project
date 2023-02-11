package rimp;

import javafx.fxml.FXMLLoader;
import javafx.scene.layout.Pane;

import java.io.IOException;
import java.net.URL;

/**
 * This class is a part of PPA coursework 4 - Airbnb (2020/21).
 *
 * This class represents the FXML loader that loads a specific
 *  FXML with or without a pre created controller object.
 *
 * @author A. Banko (k20071320), P-T. Safta (k20050982), S. McFadden (k20072607), P. Kurek (k20055027)
 * @version 01/04/2021
 */
public class PaneBuilder
{
    /**
     * This method creates a pane object from a fxml file with a
     *  predestined controller
     * @param filePath The file path to the FXML File
     * @param controller the controller object to be passed to the fxml loader
     * @return the pane created by loading the fxml
     */
    public static Pane createPane(String filePath, ModuleLayer.Controller controller)
    {
        Pane pane = null;

        URL url = PaneBuilder.class.getResource(filePath);
        FXMLLoader loader = new FXMLLoader(url);
        loader.setController(controller);

        try {
            pane = loader.load();
        } catch (IOException e) {
            System.out.println("Error While Trying To Create Pane From: " + filePath);
        }
        return pane;
    }

    /**
     * This method creates a pane object from a fxml file without a
     *  predestined controller
     * @param filePath The file path to the FXML File
     * @return the pane created by loading the fxml
     */
    public static Pane createPane(String filePath)
    {
        return PaneBuilder.createPane(filePath, null);
    }

}
