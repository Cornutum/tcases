//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.apache.commons.io.FileUtils;
import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.Arrays;
import static java.util.stream.Collectors.joining;

/**
 * Runs tests for {@link JUnitTestWriter}.
 */
public class JUnitTestWriterTest extends TestWriterTest
  {
  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Name </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_0()
    {
    // Given...
    String testDefName = "testDef-0";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .build();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .inPackage( "org.examples")
      .extending( "org.examples.util.BaseClass")
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    String results = toStdOut( () -> testWriter.writeTest( source, target));

    // Then
    verifyTest( testDefName, results);
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Any-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> None </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> Any-Name </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_1() throws Exception
    {
    // Given...
    String testDefName = "testDef-1";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .path( "/posts")
      .operation( "trace")
      .build();

    ByteArrayOutputStream outStream = new ByteArrayOutputStream();
    File outDir = new File( getResourceDir(), testDefName);
    FileUtils.deleteQuietly( outDir);
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( "someTime-tests** (* more || <")
      .toStream( outStream)
      .inDir( outDir)
      .inPackage( "org.examples")
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    String outStreamResults = outStream.toString( "UTF-8");
    assertThat( "Output stream empty", outStreamResults.isEmpty(), is( true));
    
    String outFileName = "SomeTimeTestsMoreTest.java";
    File outFile = new File( outDir, outFileName);
    String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
    verifyTest( testDefName, outFileResults);
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Class </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_2() throws Exception
    {
    // Given...
    String testDefName = "testDef-2";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .path( "/posts/{post-id}")
      .build();

    String outFileName = "TestSomething";
    Class<?> baseClass = org.cornutum.tcases.openapi.OpenApiTest.class;
    String baseClassPath = Arrays.stream( baseClass.getPackage().getName().split( "\\.")).collect( joining( "/"));
    File outDir = new File( getResourceDir(), String.format( "src/test/java/%s", baseClassPath));
    outDir.mkdirs();

    JavaTestTarget target =
      JavaTestTarget.builder()
      .toFile( new File( outDir, outFileName))
      .extending( baseClass)
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    File outFile = new File( target.getFile().getParentFile(), outFileName + ".java");
    String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
    verifyTest( testDefName, outFileResults);
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Name </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_3() throws Exception
    {
    // Given...
    String testDefName = "testDef-3";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .operation( "GET")
      .build();

    ByteArrayOutputStream outStream = new ByteArrayOutputStream();

    String outFileName = "jsonPlaceHolder_0_16_0";
    File outDir = new File( String.format( "%s/src/main/java/org/cornutum", testDefName));

    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( "api-0.16.0.json")
      .toStream( outStream)
      .toFile( outFileName)
      .inDir( outDir)
      .inPackage( "org.examples")
      .extending( "org.cornutum.tcases.openapi.OpenApiTest")
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    try
      {
      outDir.mkdirs();
      testWriter.writeTest( source, target);

      // Then
      String outStreamResults = outStream.toString( "UTF-8");
      assertThat( "Output stream empty", outStreamResults.isEmpty(), is( true));

      File outFile = new File( outDir, outFileName + "Test.java");
      String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
      verifyTest( testDefName, outFileResults);
      }
    finally
      {
      FileUtils.deleteQuietly( outDir);
      }
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_4() throws Exception
    {
    // Given...
    String testDefName = "testDef-4";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .path( "/posts")
      .build();

    File outFile = new File( getResourceDir(), "tests@cornutum.org+more$$");
    outFile.getParentFile().mkdirs();
    
    File outDir = new File( getResourceDir(), String.format( "%s/src/main/java/org/cornutum", testDefName));
    FileUtils.deleteQuietly( outDir);
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .toFile( outFile)
      .inDir( outDir)
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    File resultsFile = new File( outDir, outFile.getName());
    String outFileResults = FileUtils.readFileToString( resultsFile, "UTF-8");
    verifyTest( testDefName, outFileResults);
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Any-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Class </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_5() throws Exception
    {
    // Given...
    String testDefName = "testDef-5";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .path( "/posts/{post-id}")
      .operation( "put")
      .build();

    ByteArrayOutputStream outStream = new ByteArrayOutputStream();
    
    String outFileName = "TestJsonPlaceholder.java";
    File outFile = new File( testDefName, outFileName);
    FileUtils.deleteQuietly( outFile.getParentFile());
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( "ApiTests")
      .toStream( outStream)
      .toFile( outFile)
      .inPackage( org.cornutum.tcases.openapi.OpenApiTest.class)
      .extending( org.cornutum.tcases.openapi.OpenApiTest.class)
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    try
      {
      testWriter.writeTest( source, target);

      // Then
      String outStreamResults = outStream.toString( "UTF-8");
      assertThat( "Output stream empty", outStreamResults.isEmpty(), is( true));

      String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
      verifyTest( testDefName, outFileResults);
      }
    finally
      {
      FileUtils.deleteQuietly( outFile.getParentFile());
      }
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Name </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> No </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_6() throws Exception
    {
    // Given...
    String testDefName = "testDef-6";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .build();

    ByteArrayOutputStream outStream = new ByteArrayOutputStream();
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .toStream( outStream)
      .inPackage( "org.examples")
      .extending( "org.examples.util.ExampleTest")
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    testWriter.writeTest( source, target);

    // Then
    String outStreamResults = outStream.toString( "UTF-8");
    verifyTest( testDefName, outStreamResults);
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> Relative </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_7() throws Exception
    {
    // Given...
    String testDefName = "testDef-7";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .operation( "POST")
      .build();

    File outDir = new File( String.format( "%s/java/org/cornutum", testDefName));
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .named( "jsonPlaceholder_API")
      .inDir( outDir)
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    try
      {
      outDir.mkdirs();
      testWriter.writeTest( source, target);

      // Then
      File outFile = new File( outDir, "JsonPlaceholder_APITest.java");
      String outFileResults = FileUtils.readFileToString( outFile, "UTF-8");
      verifyTest( testDefName, outFileResults);
      }
    finally
      {
      FileUtils.deleteQuietly( outDir);
      }
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. writeTest (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> <FONT color="red"> Undefined  </FONT> </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> Class </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_8() throws Exception
    {
    // Given...
    String testDefName = "testDef-8";
    
    TestSource source =
      TestSource.from( requestTestDefFor( testDefName))
      .path( "/posts")
      .build();

    File outFile = new File( getResourceDir(), testDefName);
    
    JavaTestTarget target =
      JavaTestTarget.builder()
      .toFile( outFile)
      .extending( org.cornutum.tcases.openapi.OpenApiTest.class)
      .build();

    JUnitTestWriter testWriter = new JUnitTestWriter( new MockTestCaseWriter());
    
    // When...
    assertTestWriterException(
      () -> testWriter.writeTest( source, target),
      "Can't write test=TestDef8",
      String.format( "No package defined for target=%s", target));
    }
  }
