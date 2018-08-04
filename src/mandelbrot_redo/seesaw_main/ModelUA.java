package mandelbrot_redo.seesaw_main;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class ModelUA {
    private String floor;

    public ModelUA(String floor) {
        this.floor = floor;
    }

    public String getFloor() {
        return this.floor;
    }

    public static void main(String[] args) {
        // Unsorted floor strings
        List<String> floorStrs =
                Arrays.asList("R/C",
                              "+ 1º",
                              "R/C",
                              "+ 2º",
                              "");

        // Wrap each floor string in a ModelUA
        List<ModelUA> models = floorStrs.stream().map(ModelUA::new).collect(Collectors.toList());

        models.sort(new Comparator<ModelUA>() {
            @Override
            public int compare(ModelUA m1, ModelUA m2) {
                // Just delegate to the string compare method
                return m1.getFloor().compareTo(m2.getFloor());
            }
        });

        for (ModelUA m : models) {
            System.out.println(m.getFloor());
        }
    }
}

