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
      .set( schema( STRING))
      .add(
        value( "empty").properties( patternEmpty).set( schema().constant( "")),
        value( "unquotedSingle").set( schema().pattern( "^[^\\s\"]$")),
        value( "unquotedMany").properties( patternMany).set( schema().pattern( "^[^\\s\"]+$").minLength( 2).maxLength( 16)),
        value( "quoted").properties( patternMany).set( schema().pattern( "^\"[^\\s\"]+\"$").minLength( 2).maxLength( 16)),
        value( "quotedEmpty").properties( patternEmpty).set( schema().constant( "\"\"")),
        value( "quotedBlanks").properties( patternMany).set( schema().pattern( "^\"[^\\s\"]*( +[^\\s\"]*)+\"$").minLength( 2).maxLength( 16)),
        value( "quotedQuotes").properties( patternMany).set( schema().pattern( "^\"[^\\s\"]*(\"{2}[^\\s\"]*)+\"$").minLength( 2).maxLength( 16)))
      .build();

    VarDef fileNameVar =
      var( "fileName")
      .set( schema( STRING))
      .add(
        value( "defined").properties( fileName),
        failureValue( "missing"))
      .build();

    VarDef fileExistsVar =
      var( "exists")
      .set( schema( BOOLEAN))
      .add(
        value( true).properties( fileExists),
        failureValue( false))
      .build();

    VarDef linesLongerThanPatternVar =
      var( "linesLongerThanPattern")
      .set( schema( INTEGER).format( "int32"))
      .add(
        onceValue( 1).properties( matchable),
        value( "many").properties( matchable).set( schema( INTEGER).minimum( 2).maximum( 32)),
        failureValue( 0).when( has( patternMany)))
      .build();

    VarDef patternMatchesVar =
      var( "patternMatches")
      .when( has( matchable))
      .set( schema( INTEGER).format( "int32"))
      .add(
        onceValue( 0),
        value( 1).properties( match),
        value( "many").properties( match, matchMany).set( schema( INTEGER).minimum( 2).maximum( 16)))
      .build();

    VarDef patternsInLineVar =
      var( "patternsInLine")
      .when( has( match))
      .set( schema( INTEGER).format( "int32"))
      .add(
        value( 1),
        value( "many").when( has( matchMany)).set( schema( INTEGER).minimum( 2).maximum( 4))) 
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
      .add( function( "find").vars( patternVar, fileNameVar, fileVar))
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
