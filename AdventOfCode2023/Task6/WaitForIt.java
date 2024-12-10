import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class WaitForIt {
    
    private static final String FILEPATH = "Task6/input.txt";
    // private static final String FILEPATH = "Task6/controllInput.txt";

    public static void main(String[] args) {
        TimeTable tt = importTimeTable();
        System.out.println(tt.numOfWinningChargeTimes());
    }

    private static long productOfNumWinningChargeTimes(TimeTable tt) {
        long prod = 1;
        for (long num : tt.numOfWinningChargeTimesOLD()) {
            prod *= num;
        }
        return prod;
    }

    private static TimeTable importTimeTable() {
        List<String> input = new ArrayList<>();
        try(BufferedReader br = new BufferedReader(new FileReader(FILEPATH))) {
            for (String line; (line = br.readLine()) != null;) {
                input.add(String.join("", line.substring(9).trim().split(" +")));
            }
            return new TimeTable(input);
            // return new TimeTable(List.of(reformat(input)));
        } catch (IOException e) {
            // TODO: handle exception
            throw new IllegalArgumentException("IO!");
        }
    }



    // form: 
    //      time1, time2, time3, ...
    //      dist1, dist2, dist3, ...
    // to: 
    //      time1, dist1
    //      time2, dist2
    //      ...
    private static String[][] reformat(List<String> input) {
       String[][] reformatted = new String[input.get(0).split(" +").length][2];
        for (int i = 0; i < input.size(); i++) {
            String vals[] = input.get(i).split(" +");
            for (int j = 0; j < vals.length; j++) {
                reformatted[j][i] = vals[j];
            }
        }
        return reformatted;
    }
}
