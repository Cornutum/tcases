//////////////////////////////////////////////////////////////////////////////
//
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.annotation.sample1;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.cornutum.tcases.annotation.*;
import org.cornutum.tcases.annotation.creator.OutputAnnotationContainer;
import org.cornutum.tcases.annotation.util.CustomToStringStyle;

import static org.cornutum.tcases.TestCase.Type.FAILURE;

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
@Function(value = "Find", having = @Has(name = "description", value = "The find function")) // name/value defaults to class name
public class FindFunction {

  // properties for value conditions
  public static final String PATTERN_EMPTY = "empty";
  public static final String PATTERN_SINGLE_CHAR = "singleChar";
  public static final String PATTERN_QUOTED = "quoted";
  public static final String FILE_EXISTS = "fileExists";

  @TestCaseId // presence is optional
  public int testCaseId;

  @IsFailure // presence is optional, will be filled based on VarDefs
  public boolean isFailure;

  @OutputAnnotations // presence is optional, will be filled based on Output annotations
  public OutputAnnotationContainer having;

  /**
   * Input argument
   */
  @VarSet(when = FILE_EXISTS)
  public Pattern pattern;

  /**
   * Input argument
   */
  @Var({@Value(value = "false",
        type = FAILURE,
        having = @Has(name = "type", value = "No Filename"))})
  public Boolean filenameDefined; // boolean always has values true, false

  /**
   * Test environment (target file)
   */
  @VarSet
  public FileWithPatternedLines file;

  /**
   * A file with contents to be searched with a pattern
   */
  // VarSet
  public static class FileWithPatternedLines {
    @Var({@Value(value = "true", properties = FILE_EXISTS),
          @Value(value = "false", type = FAILURE, having = @Has(name = "type", value = "FileNotFound"))
    })
    public Boolean exists; // boolean always has values true, false

    @VarSet
    public FileContentsWithPattern contents;

    /**
     * file content type
     */
    // Nested VarSet
    public static class FileContentsWithPattern {
      @Var(@Value(value = "false", type = FAILURE,
              having = @Has(name = "type", value = "PatternTooLong"),
              whenNot = PATTERN_EMPTY))
      public Boolean linesLongerThanPattern;

      @Var
      public CardinalityZeroToN patterns;

      @Var
      public PatternsInLine patternsInLine;

      public enum PatternsInLine {
        ONE,
        MANY;
      }
    }
  }

  // VarSet
  public static class Pattern {
    @Var({@Value(value = "NONE", properties = {PATTERN_EMPTY}),
          @Value(value = "ONE", properties = {PATTERN_SINGLE_CHAR})
    })
    public CardinalityZeroToN size;

    @Var({@Value(value = "YES", properties = PATTERN_QUOTED, whenNot = PATTERN_EMPTY),
          @Value(value = "NO", whenNot = PATTERN_EMPTY),
          @Value(value = "UNTERMINATED", type = FAILURE, whenNot = PATTERN_EMPTY, having = @Has(name = "type", value = "QuoteMismatch"))
    })
    public QuotedType quoted;

    @Var(value = {
           @Value(value = "ONE", when = {PATTERN_QUOTED, PATTERN_SINGLE_CHAR}),
           @Value(value = "MANY", when = {PATTERN_QUOTED}, whenNot = {PATTERN_SINGLE_CHAR})
         },
           whenNot = PATTERN_EMPTY)
    public CardinalityZeroToN blanks;

    @Var(whenNot = {PATTERN_EMPTY, PATTERN_SINGLE_CHAR})
    public CardinalityZeroToN embeddedQuotes;

    public enum QuotedType {
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
