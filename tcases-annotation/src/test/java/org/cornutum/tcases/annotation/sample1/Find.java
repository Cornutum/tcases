package org.cornutum.tcases.annotation.sample1;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.annotation.*;
import org.cornutum.tcases.annotation.util.CustomToStringStyle;

@Function // name/value defaults to class name
public class Find {

  @TestCaseId // presence is optional
  public int testCaseId;

  @IsFailure // presence is optional, will be filled based on VarDefs
  public boolean isFailure;

  private Pattern pattern;

  @Var(values = {@Value(value = "false", type = TestCase.Type.FAILURE)})
  private Boolean filenameDefined; // boolean always has values true, false

  private FileWithPatternedLines file;

  // VarSet
  public static class FileWithPatternedLines {
    @Var(values = {@Value(value = "false", type = TestCase.Type.FAILURE)})
    private Boolean exists; // boolean always has values true, false

    private FileContentsWithPattern contents;

  }

  // VarSet
  public static class FileContentsWithPattern {
    @Var(values = {@Value(value = "NONE", type = TestCase.Type.FAILURE)})
    private CardinalityZeroToN linesLongerThanPattern;

    @Var
    private CardinalityZeroToN patterns;

    @Var
    private PatternsInLine patternsInLine;

    public enum PatternsInLine {
      ONE,
      MANY;
    }
  }

  // VarSet
  public static class Pattern {
    @Var
    private CardinalityZeroToN size;

    @Var(values = {@Value(value = "UNTERMINATED", type = TestCase.Type.FAILURE)})
    private QuotedType quoted;

    @Var
    private CardinalityZeroToN blanks;

    @Var
    private CardinalityZeroToN embeddedQuotes;

    private enum QuotedType {
      YES,
      NO,
      UNTERMINATED;
    }

  }

  // ValueSet, reusable
  public enum CardinalityZeroToN {
    NONE,
    ONE,
    MANY;
  }

  @Override
  public String toString() {
    return ReflectionToStringBuilder.toString(this, CustomToStringStyle.INSTANCE);
  }
}
