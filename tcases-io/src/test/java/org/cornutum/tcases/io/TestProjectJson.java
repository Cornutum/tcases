//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.hamcrest.ExpectedFailure.Failable;
import org.cornutum.tcases.SystemInputDefMatcher;
import org.cornutum.tcases.SystemTestDefMatcher;
import org.cornutum.tcases.generator.io.GeneratorSetException;
import org.cornutum.tcases.generator.io.GeneratorSetResources;
import org.cornutum.tcases.generator.io.TestGeneratorSetJson;

import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import org.junit.Test;
import org.leadpony.justify.api.JsonValidatingException;
import org.leadpony.justify.api.Problem;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for the {@link ProjectJsonWriter} and {@link ProjectJsonReadser}.
 *
 */
public class TestProjectJson
  {
  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Value </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> Relative </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_0()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-set-once.xml";
    String baseTestsResource = "system-test-def-find.json";

    Project project =
      ProjectBuilder.with( systemInputResource_.read( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .baseTestRef( getResourceFileRelative( baseTestsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertThat( "System input", projectAfter.getSystemInput(), matches( new SystemInputDefMatcher( systemInputResource_.read( systemInputResource))));
    assertThat( "Generators", projectAfter.getGenerators(), is( generatorSetResource_.read( generatorsResource)));
    assertThat( "Base tests", projectAfter.getBaseTests(), matches( new SystemTestDefMatcher( systemTestResource_.readJson( baseTestsResource))));
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Relative </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_1()
    {
    // Given...
    String systemInputResource = "system-input-def-find.json";
    String generatorsResource = "generator-set-once.xml";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .refBase( getResourceDir( TestGeneratorSetJson.class))
      .generatorsRef( getResourceFileRelative( generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertThat( "System input", projectAfter.getSystemInput(), matches( new SystemInputDefMatcher( systemInputResource_.readJson( systemInputResource))));
    assertThat( "Generators", projectAfter.getGenerators(), is( generatorSetResource_.read( generatorsResource)));
    assertThat( "Base tests", projectAfter.getBaseTests(), is( nullValue()));
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Value </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Value </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_2()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String baseTestsResource = "system-test-def-2.xml";

    Project project =
      ProjectBuilder.with( systemInputResource_.read( systemInputResource))
      .baseTests( systemTestResource_.read( baseTestsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertThat( "System input", projectAfter.getSystemInput(), matches( new SystemInputDefMatcher( systemInputResource_.read( systemInputResource))));
    assertThat( "Generators", projectAfter.getGenerators(), is( nullValue()));
    assertThat( "Base tests", projectAfter.getBaseTests(), matches( new SystemTestDefMatcher( systemTestResource_.read( baseTestsResource))));
    }


  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Relative </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Value </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_3()
    {
    // Given...
    String systemInputResource = "system-input-def-find.json";
    String generatorsResource = "generator-set-sample.json";
    String baseTestsResource = "system-test-def-2.xml";

    Project project =
      ProjectBuilder.with( getResourceFileRelative( systemInputResource))
      .refBase( getResourceDir())
      .generators( generatorSetResource_.readJson( generatorsResource))
      .baseTestRef( getResourceFile( baseTestsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertThat( "System input", projectAfter.getSystemInput(), matches( new SystemInputDefMatcher( systemInputResource_.readJson( systemInputResource))));
    assertThat( "Generators", projectAfter.getGenerators(), is( generatorSetResource_.readJson( generatorsResource)));
    assertThat( "Base tests", projectAfter.getBaseTests(), matches( new SystemTestDefMatcher( systemTestResource_.read( baseTestsResource))));
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_4()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-combiner-property-extra.json";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getGenerators(), GeneratorSetException.class, "Invalid generator set definition");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> <FONT color="red"> Not-URL  </FONT> </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_5()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-set-once.xml";

    Project project =
      ProjectBuilder.with( getResourceFileRelative( systemInputResource))
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getSystemInput(), SystemInputException.class, "Can't read resource at " + systemInputResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_6()
    {
    assertValidationFailure
      ( "project-generator-ref-invalid.json",
        "The value must be of object type, but actual type is string",
        "The value must be a valid URI reference");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Value </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_7()
    {
    assertFailure( () -> readJson( "project-base-tests-invalid.json"), ProjectException.class, "Error reading base tests definition");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> <FONT color="red"> Not-URL  </FONT> </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_8()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-set-once.xml";
    String baseTestsResource = "system-test-def-2.xml";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .baseTestRef( getResourceFileRelative( baseTestsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getBaseTests(), SystemTestException.class, "Can't read resource at " + baseTestsResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> <FONT color="red"> Yes  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_9()
    {
    // properties = generatorsReference,inputDefReference,refBase

    // Given...
    //
    //   refBase = Valid
    //
    //   inputDef.definedBy = Reference
    //
    //   inputDef.uri = Absolute
    //
    //   inputDef.value = Valid
    //
    //   generators.definedBy = Reference
    //
    //   generators.uri = Absolute
    //
    //   generators.value = Valid
    //
    //   baseTests.definedBy = None
    //
    //   baseTests.uri = (not applicable)
    //
    //   baseTests.value = (not applicable)
    //
    //   additionalProperties = Yes
    
    // When...

    // Then...
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_10()
    {
    // Given...
    String systemInputResource = "system-input-var-name-invalid.json";
    String generatorsResource = "generator-set-once.xml";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getSystemInput(), SystemInputException.class, "Error processing Things, Make, a.b.c.d");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_11()
    {
    assertValidationFailure
      ( "project-input-ref-invalid.json",
        "The value must be of object type, but actual type is string",
        "The value must be a valid URI reference");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_12()
    {
    assertValidationFailure
      ( "project-base-tests-ref-invalid.json",
        "The value must be of object type, but actual type is string",
        "The value must be a valid URI reference");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> <FONT color="red"> Not-Found  </FONT> </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_13() throws Exception
    {
    // Given...
    String systemInputResource = "No-Such-File.xml";
    String generatorsResource = "generator-set-once.xml";

    Project project =
      ProjectBuilder.with( new URI( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getSystemInput(), SystemInputException.class, "Can't read resource at " + systemInputResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> <FONT color="red"> Not-Found  </FONT> </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_14() throws Exception
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-set-once.xml";
    String baseTestsResource = "Nope";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( getResourceFile( TestGeneratorSetJson.class, generatorsResource))
      .baseTestRef( new URI( baseTestsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getBaseTests(), SystemTestException.class, "Can't read resource at " + baseTestsResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Undefined </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> <FONT color="red"> Not-URL  </FONT> </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_15()
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "generator-set-once.xml";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .generatorsRef( getResourceFileRelative( generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getGenerators(), GeneratorSetException.class, "Can't read resource at " + generatorsResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> <FONT color="red"> None  </FONT> </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_16()
    {
    assertValidationFailure
      ( "project-input-missing.json",
        "The object must have a property whose name is \"inputDef\"");
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> Valid </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> <FONT color="red"> Not-Found  </FONT> </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_17() throws Exception
    {
    // Given...
    String systemInputResource = "system-input-def-0.xml";
    String generatorsResource = "Zilch";

    Project project =
      ProjectBuilder.with( getResourceFile( systemInputResource))
      .refBase( getResourceDir())
      .generatorsRef( new URI( generatorsResource))
      .build();

    // When...
    Project projectAfter = fromJson( project);

    // Then...
    assertThat( "From JSON", projectAfter, matches( new ProjectMatcher( project)));
    assertFailure( () -> projectAfter.getGenerators(), GeneratorSetException.class, "Can't read resource at " + generatorsResource);
    }

  /**
   * Tests ProjectJsonReader and ProjectJsonWriter using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18. (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> refBase </TD> <TD> <FONT color="red"> Invalid  </FONT> </TD> </TR>
   * <TR><TD> inputDef.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> inputDef.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> inputDef.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> generators.definedBy </TD> <TD> Reference </TD> </TR>
   * <TR><TD> generators.uri </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> generators.value </TD> <TD> Valid </TD> </TR>
   * <TR><TD> baseTests.definedBy </TD> <TD> None </TD> </TR>
   * <TR><TD> baseTests.uri </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> baseTests.value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additionalProperties </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void testProjectJson_18()
    {
    assertValidationFailure
      ( "project-refbase-invalid.json",
        "The value must be a valid URI.");
    }

  /**
   * Returns the result of reading the JSON produced by writing the given project as a JSON document.
   */
  public Project fromJson( Project projectBefore)
    {
    ByteArrayOutputStream projectOut = new ByteArrayOutputStream();
    try( ProjectJsonWriter writer = new ProjectJsonWriter( projectOut))
      {
      writer.write( projectBefore);
      }

    Project projectAfter;
    ByteArrayInputStream projectIn = new ByteArrayInputStream( projectOut.toByteArray());
    try( ProjectJsonReader reader = new ProjectJsonReader( projectIn))
      {
      projectAfter = reader.getProject();
      }    

    return projectAfter;
    }

  /**
   * Reports an error if the given Failable does not produce the expected exception.
   */
  @SuppressWarnings("unchecked")
  private <F extends Throwable> void assertFailure( Failable action, Class<F> failureType, String causedBy)
    {
    expectFailure( failureType)
      .when( action)
      .then( failure -> {
        while( failureType.isInstance( failure.getCause()))
          {
          failure = (F) failure.getCause();
          }
        assertThat( "Cause", failure.getMessage(), is( causedBy));
        });
    }

  public void assertValidationFailure( String projectResource, String... expected)
    {
    expectFailure( ProjectException.class)
      .when( () -> readJson( projectResource))
      .then( failure -> {
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause.getClass(), equalTo( JsonValidatingException.class));

        JsonValidatingException jve = (JsonValidatingException)cause;
        List<String> problems = problems( jve).map( p -> p.getMessage()).collect( toList());
        assertThat( "Problems", problems, containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
    }

  private Stream<Problem> problems( JsonValidatingException jve)
    {
    return jve.getProblems().stream().flatMap( p -> problems( p));
    }

  private Stream<Problem> problems( Problem problem)
    {
    return
      problem.hasBranches()?
      
      IntStream.range( 0, problem.countBranches())
      .mapToObj( i -> problem.getBranch(i))
      .flatMap( problems -> problems.stream().flatMap( p -> problems( p))) :

      Stream.of( problem);
    }

  /**
   * Returns the {@link Project} defined by the given JSON resource.
   */
  public Project readJson( String resource)
    {
    Project project  = null;
    InputStream stream = null;
    
    stream = TestProjectJson.class.getResourceAsStream( resource);
    if( stream == null)
      {
      throw
        new RuntimeException
        ( "Can't find resource=" + TestProjectJson.class.getName() + "." + resource);
      }

    try( ProjectJsonReader reader = new ProjectJsonReader( stream))
      {
      project = reader.getProject();
      }

    return project;
    }

  /**
   * Returns the location of the resource directory for this class.
   */
  private URI getResourceDir()
    {
    return getResourceDir( getClass());
    }

  /**
   * Returns the location of the resource directory for the given class.
   */
  private URI getResourceDir( Class<?> resourceClass)
    {
    return getResourceFile( resourceClass, ".");
    }

  /**
   * Returns the location of the resource file for this class.
   */
  private URI getResourceFile( String resourceName)
    {
    return getResourceFile( getClass(), resourceName);
    }

  /**
   * Returns the location of the resource file for the given class.
   */
  private URI getResourceFile( Class<?> resourceClass, String resourceName)
    {
    try
      {
      return resourceClass.getResource( resourceName).toURI();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get location of resource=" + resourceName, e);
      }
    }

  /**
   * Returns the relative location of the given resource file.
   */
  private URI getResourceFileRelative( String resourceName)
    {
    try
      {
      return new URI( resourceName);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get relative location of resource=" + resourceName, e);
      }
    }

  private SystemInputResources systemInputResource_ = new SystemInputResources( TestSystemInputJson.class);
  private SystemTestResources systemTestResource_ = new SystemTestResources( TestSystemTestDocWriter.class);
  private GeneratorSetResources generatorSetResource_ = new GeneratorSetResources( TestGeneratorSetJson.class);
  }
