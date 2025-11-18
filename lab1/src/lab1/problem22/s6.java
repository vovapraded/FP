package lab1.problem22;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

public class s6 {
    public static void main(String[] args) throws IOException {
        String content = Files.readString(Paths.get("resources/names.txt"));

        String[] names = parseNames(content);



        long result = solve(names);
        System.out.println(result);
    }

    public static long solve(String[] names) {
        Arrays.sort(names);
        long result = 0;
        for (int i = 0; i < names.length; i++) {
            result += (long) (i + 1) * getNameScore(names[i]);
        }
        return result;
    }

    private static int getNameScore(String name) {
        int score = 0;
        for (int i = 0; i < name.length(); i++) {
            score += name.charAt(i) - 'A' + 1;
        }
        return score;
    }

    public static String[] parseNames(String content) {
        content = content.trim().replaceAll("\\s+", "");

        String[] rawNames = content.split(",");

        String[] names = new String[rawNames.length];
        for (int i = 0; i < rawNames.length; i++) {
            names[i] = rawNames[i].replace("\"", "");
        }

        return names;
    }


}
