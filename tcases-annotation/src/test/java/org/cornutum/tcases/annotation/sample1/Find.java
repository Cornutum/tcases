package org.cornutum.tcases.annotation.sample1;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.cornutum.tcases.TestCase;
import org.cornutum.tcases.annotation.*;
import org.cornutum.tcases.annotation.util.CustomToStringStyle;

/**
 * Usage: find pattern file
 *
 * Locates one or more instances of a given pattern in a text file.
 *
 * All lines in the file that contain the pattern are written to standard output.
 * A line containing the pattern is written only once,
 * regardless of the number of times the pattern occurs in it.
 *
 * The pattern is any sequence of characters whose length does not exceed
 * the maximum length of a line in the file.
 * To include a blank in the pattern, the entire pattern must be enclosed in quotes (").
 * To include a quotation mark in the pattern, two quotes in a row ("") must be used.
 */
@Function // name/value defaults to class name
public class Find {

  // properties for value conditions
  public static final String PATTERN_EMPTY = "empty";
  public static final String PATTERN_SINGLE_CHAR = "singleChar";
  public static final String PATTERN_QUOTED = "quoted";
  public static final String FILE_EXISTS = "fileExists";

  @TestCaseId // presence is optional
  public int testCaseId;

  @IsFailure // presence is optional, will be filled based on VarDefs
  public boolean isFailure;

  /**
   * Input argument
   */
  @VarSet(when = FILE_EXISTS)
  private Pattern pattern;

  /**
   * Input argument
   */
  @Var(values = {@Value(value = "false", type = TestCase.Type.FAILURE)})
  private Boolean filenameDefined; // boolean always has values true, false

  /**
   * Test environment (target file)
   */
  @VarSet
  private FileWithPatternedLines file;

  /**
   * A file with contents to be searched with a pattern
   */
  // VarSet
  public static class FileWithPatternedLines {
    @Var(values = {
            @Value(value = "true", properties = FILE_EXISTS),
            @Value(value = "false", type = TestCase.Type.FAILURE)
    })
    private Boolean exists; // boolean always has values true, false

    @VarSet
    private FileContentsWithPattern contents;

    /**
     * file content type
     */
    // Nested VarSet
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
  }

  // VarSet
  public static class Pattern {
    @Var(values = {
            @Value(value = "NONE", properties = {PATTERN_EMPTY}),
            @Value(value = "ONE", properties = {PATTERN_SINGLE_CHAR})
    })
    private CardinalityZeroToN size;

    @Var(values = {
            @Value(value = "YES", properties = PATTERN_QUOTED),
            @Value(value = "NO", whenNot = PATTERN_EMPTY),
            @Value(value = "UNTERMINATED", type = TestCase.Type.FAILURE)
    })
    private QuotedType quoted;

    @Var(values = {
            @Value(value = "ONE", when = {PATTERN_QUOTED, PATTERN_SINGLE_CHAR}),
            @Value(value = "MANY", when = {PATTERN_QUOTED}, whenNot = {PATTERN_SINGLE_CHAR})
    }, whenNot = PATTERN_EMPTY)
    private CardinalityZeroToN blanks;

    @Var(whenNot = {PATTERN_EMPTY, PATTERN_SINGLE_CHAR})
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
