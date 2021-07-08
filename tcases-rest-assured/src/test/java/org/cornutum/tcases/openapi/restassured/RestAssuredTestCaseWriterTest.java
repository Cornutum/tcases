//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.restassured;

import org.apache.commons.io.FileUtils;
import org.cornutum.tcases.openapi.moco.MocoServerConfig;
import org.cornutum.tcases.openapi.moco.MocoServerTest;
import org.cornutum.tcases.openapi.moco.MocoServerTestWriter;
import org.cornutum.tcases.openapi.moco.RestServerTestWriter;
import org.cornutum.tcases.openapi.testwriter.JUnitTestWriter;
import org.cornutum.tcases.openapi.testwriter.JavaTestTarget;
import org.cornutum.tcases.openapi.testwriter.TestSource;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueJson;
import org.cornutum.tcases.openapi.testwriter.encoder.DataValueText;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Runs tests for {@link RestAssuredTestCaseWriter}
 */
public class RestAssuredTestCaseWriterTest extends MocoServerTest
  {
  @Test
  public void writeOpenApiTest() throws Exception
    {
    // Given...
    String testDefName = "OpenApiTest";
    
    TestSource source =
      TestSource.from( stdRequestTestDef( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( testDefName)
      .inDir( getGeneratedTestDir())
      .build();

    MocoServerConfig serverConfig = MocoServerConfig.resource( stdMocoServerConfig( testDefName)).build();

    MocoServerTestWriter testWriter = new RestServerTestWriter( serverConfig, new RestAssuredTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    verifyTest( testDefName, FileUtils.readFileToString( testWriter.getTestFile( source, target), "UTF-8"));
    }
  
  @Test
  public void writeOpenApiAuth() throws Exception
    {
    // Given...
    String testDefName = "OpenApiAuth";
    
    TestSource source =
      TestSource.from( stdRequestTestDef( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( testDefName)
      .inDir( getGeneratedTestDir())
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new RestAssuredTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    verifyTest( testDefName, FileUtils.readFileToString( testWriter.getTestFile( source, target), "UTF-8"));
    }

  @Test
  public void whenGetConverter()
    {
    // Given...
    RestAssuredTestCaseWriter writer = new RestAssuredTestCaseWriter();

    // Then...
    assertEquals( "text/xml", DataValueText.class, converterFor( writer, "text/xml"));
    assertEquals( "text/json", DataValueText.class, converterFor( writer, "text/json"));
    assertEquals( "text/javascript+json", DataValueText.class, converterFor( writer, "text/javascript+json"));

    assertEquals( "application/json", DataValueJson.class, converterFor( writer, "application/json"));
    assertEquals( "application/*+json", DataValueJson.class, converterFor( writer, "application/*+json"));
    assertEquals( "application/openapi+json;version=3.0.2", DataValueJson.class, converterFor( writer, "application/*+json"));

    assertEquals( "example/*", DataValueText.class, converterFor( writer, "example/*"));
    assertEquals( "*/*", DataValueText.class, converterFor( writer, "*/*"));
    }

  private Class<?> converterFor( RestAssuredTestCaseWriter writer, String mediaType)
    {
    return writer.getConverter( mediaType).map( Object::getClass).orElse( null);
    }
  }
