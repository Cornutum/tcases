//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.VarDef;
import org.cornutum.tcases.VarSet;

import static org.cornutum.tcases.SystemInputs.*;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.resolve.DataValue.Type.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.net.URL;

/**
 * Runs tests for {@link SystemInputs} builder interfaces.
 */
public class SystemInputBuilderTest
  {
  @Test
  public void buildExamplesFind() throws Exception
    {
    // Given...
    SystemInputDef examplesExpected =
      systemInputResources_.readJson(
        new URL( "http://www.cornutum.org/tcases/docs/examples/json/find-Input.json")
        .openStream());
    
    // When...

    // Define property names
    String fileExists = "fileExists";
    String fileName = "fileName";
    String match = "match";
    String matchMany = "matchMany";
    String matchable = "matchable";
    String patternEmpty = "patternEmpty";
    String patternMany = "patternMany";

    // Create variable definitions
    VarDef patternVar =
      var( "pattern")
      .when( has( fileExists))
      .schema( schema( STRING).build())

      .values(

        value( "empty")
        .properties( patternEmpty)
        .schema( schema().constant( "").build())
        .build(),

        value( "unquotedSingle")
        .schema( schema().pattern( "^[^\\s\"]$").build())
        .build(),

        value( "unquotedMany")
        .properties( patternMany)
        .schema( schema().pattern( "^[^\\s\"]+$").minLength( 2).maxLength( 16).build())
        .build(),

        value( "quoted")
        .properties( patternMany)
        .schema( schema().pattern( "^\"[^\\s\"]+\"$").minLength( 2).maxLength( 16).build())
        .build(),

        value( "quotedEmpty")
        .properties( patternEmpty)
        .schema( schema().constant( "\"\"").build())
        .build(),

        value( "quotedBlanks")
        .properties( patternMany)
        .schema( schema().pattern( "^\"[^\\s\"]*( +[^\\s\"]*)+\"$").minLength( 2).maxLength( 16).build())
        .build(),

        value( "quotedQuotes")
        .properties( patternMany)
        .schema( schema().pattern( "^\"[^\\s\"]*(\"{2}[^\\s\"]*)+\"$").minLength( 2).maxLength( 16).build())
        .build())
      .build();

    VarDef fileNameVar =
      var( "fileName")
      .schema( schema( STRING).build())
      .values(

        value( "defined")
        .properties( fileName)
        .build(),

        failureValue( "missing")
        .build())
      .build();

    VarDef fileExistsVar =
      var( "exists")
      .schema( schema( BOOLEAN).build())
      .values(
        value( true)
        .properties( fileExists)
        .build(),

        failureValue( false)
        .build())
      .build();

    VarDef linesLongerThanPatternVar =
      var( "linesLongerThanPattern")
      .schema( schema( INTEGER).format( "int32").build())
      .values(
        onceValue( 1)
        .properties( matchable)
        .build(),

        value( "many")
        .properties( matchable)
        .schema( schema( INTEGER).minimum( 2).maximum( 32).build())
        .build(),

        failureValue( 0)
        .when( has( patternMany))
        .build())
      .build();

    VarDef patternMatchesVar =
      var( "patternMatches")
      .when( has( matchable))
      .schema( schema( INTEGER).format( "int32").build())
      .values(
        onceValue( 0)
        .build(),

        value( 1)
        .properties( match)
        .build(),

        value( "many")
        .properties( match, matchMany)
        .schema( schema( INTEGER).minimum( 2).maximum( 16).build())
        .build())
      .build();

    VarDef patternsInLineVar =
      var( "patternsInLine")
      .when( has( match))
      .schema( schema( INTEGER).format( "int32").build())
      .values(
        value( 1)
        .build(),

        value( "many")
        .when( has( matchMany))
        .schema( schema( INTEGER).minimum( 2).maximum( 4).build())
        .build()) 
      .build();
    
    VarSet fileVar =
      varSet( "file")
      .type( "env")
      .when( has( fileName))
      .members(
        fileExistsVar,

        varSet( "contents")
        .when( allOf( has( fileExists), not( patternEmpty)))
        .members(
          linesLongerThanPatternVar,
          patternMatchesVar,
          patternsInLineVar)
        .build())
      
      .build();

    // Create system input definition
    SystemInputDef examples =
      system( "Examples")
      .functions(
        function( "find")
        .vars( patternVar, fileNameVar, fileVar)
        .build())
      .build();

    // Then...
    assertThat(
      "Find example",
      Tcases.getEffectiveInputDef( examples),
      matches( new SystemInputDefMatcher( Tcases.getEffectiveInputDef( examplesExpected))));

    // Given...
    SystemTestDef testsExpected = Tcases.getTests( examplesExpected, null, null);

    // When...
    SystemTestDef tests = Tcases.getTests( examples, null, null);

    // Then...
    assertThat( "Find tests", tests, matches( new SystemTestDefMatcher( testsExpected)));
    }

  private SystemInputResources systemInputResources_ = new SystemInputResources( SystemInputBuilderTest.class);
  }
