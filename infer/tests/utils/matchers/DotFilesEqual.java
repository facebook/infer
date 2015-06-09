package utils.matchers;

import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Matcher;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.Map;

public class DotFilesEqual extends BaseMatcher<File> {

  String dotFile;

  public DotFilesEqual(String dotFile) {
    this.dotFile = dotFile;
  }

  private StringBuilder diffOutput = new StringBuilder();
  private String diffCommand = "";

  @Override
  public boolean matches(Object o) {
    File newDotFile = (File) o;
    Map<String, String> env = System.getenv();
    if (env.get("INFER_DOT_REPLACE") != null) {
      diffCommand = "cp " + newDotFile.getAbsolutePath() + " " + dotFile;
    } else {
      diffCommand = "diff -u " + dotFile + " " + newDotFile.getAbsolutePath();
    }
    try {
      Process diff_process = Runtime.getRuntime().exec(diffCommand);
      try (BufferedReader inputReader =
               new BufferedReader(new InputStreamReader(diff_process.getInputStream()));
           BufferedReader errorReader =
               new BufferedReader(new InputStreamReader(diff_process.getErrorStream()))) {

        String line = inputReader.readLine();
        while (line != null) {
          diffOutput = diffOutput.append(line + "\n");
          line = inputReader.readLine();
        }
        String errorLine = errorReader.readLine();
        while (errorLine != null) {
          diffOutput = diffOutput.append(errorLine + "\n");
          errorLine = errorReader.readLine();
        }
        diff_process.waitFor();
        return "".equals(diffOutput.toString());
      } catch (Exception e) {
        e.printStackTrace();
        return false;
      }
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }
  }

  @Override
  public void describeTo(Description description) {
    description.appendText("Dot file equal to " + dotFile);
  }

  @Override
  public void describeMismatch(Object item, Description description) {
    String outputDesc = "The command:\n\n  " + diffCommand
      + "\n\nfailed with output:\n" + diffOutput.toString() + "\n";
    description.appendText(outputDesc);
  }

  public static <T> Matcher<File> dotFileEqualTo(String dotFile) {
    return new DotFilesEqual(dotFile);
  }

}
