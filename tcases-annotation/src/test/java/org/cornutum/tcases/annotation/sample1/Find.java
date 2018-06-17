package org.cornutum.tcases.annotation.sample1;

import org.cornutum.tcases.annotation.*;

@Function // name/value defaults to class name
public class Find {

  private Pattern pattern;

  @Var(values = {@Value("defined"), @Value(value = "missing", failure = true)})
  private String filename;

  private EnvFile file;

  @VarSet
  public static class EnvFile {
    @Var(values = {@Value("true"), @Value(value = "false", failure = true)})
    private Boolean exists;

    @Override
    public String toString() {
      return "EnvFile{" +
        "exists=" + exists +
        '}';
    }
  }

  @VarSet
  public static class Pattern {
    @Var(values = {
      @Value("empty"),
      @Value("singleChar"),
      @Value("manyChars")
    })
    private String size;

    @Override
    public String toString() {
      return "Pattern{" +
        "size='" + size + '\'' +
        '}';
    }
  }

  @Override
  public String toString() {
    return "Find{" +
      "pattern=" + pattern +
      ", filename='" + filename + '\'' +
      ", file=" + file +
      '}';
  }
}
