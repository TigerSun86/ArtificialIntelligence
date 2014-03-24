import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;

public class DataExtractor {
    private BufferedReader in = null;
    private final String fName;

    public DataExtractor(final String fNameIn) {
        this.fName = fNameIn;
        try {
            final URL attrURL = new URL(fName);
            in =
                    new BufferedReader(new InputStreamReader(
                            attrURL.openStream()));
        } catch (final IOException e) {
            System.err.println("Couldn't find file: " + fName);
            close();
        }
    }

    public final String[] nextLine () {
        String inputLine = null;
        try {
            inputLine = in.readLine();
        } catch (final IOException e) {
            System.err.println("Couldn't read file: " + fName);
            close();
        }

        if (inputLine == null) {
            // End of file.
            return null;
        }

        if (inputLine.length() <= 1) {
            System.err.println("Invalid line: " + inputLine);
            return null;
        }

        final String[] result = inputLine.split(" ");
        if (result.length <= 1) {
            System.err.println("No enough data: " + inputLine);
            return null;
        }

        return result;
    }

    public final void close () {
        if (in != null) {
            try {
                in.close();
            } catch (final IOException e) {
                System.err.println("Couldn't close file: " + fName);
            }
        }
    }

}
