//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.testwriter;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Optional;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.resolver.RequestTestDef;
import org.cornutum.tcases.openapi.resolver.io.RequestTestDefReader;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.fail;

/**
 * Runs tests for {@link JUnitTestWriter}.
 */
public class JUnitTestWriterTest
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
  public void writeTest_1()
    {
    // properties = dir,op,path,testName

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Any-Name
    //
    //   Source.Request-Cases.Count = None
    //
    //   Source.Path = Defined
    //
    //   Source.Operation = Defined
    //
    //   Target.Test-Name.Defined = Yes
    //
    //   Target.Test-Name.Value = Any-Name
    //
    //   Target.Output-Stream.Defined = Yes
    //
    //   Target.Output-File.Defined = No
    //
    //   Target.Output-File.Path = (not applicable)
    //
    //   Target.Output-File.Dir.Exists = (not applicable)
    //
    //   Target.Output-File.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Output-File.Name = (not applicable)
    //
    //   Target.Output-Dir.Defined = Yes
    //
    //   Target.Output-Dir.Path = Absolute
    //
    //   Target.Output-Dir.Dir.Exists = No
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = No
    //
    //   Target.Package = Defined
    //
    //   Target.BaseClass.Defined = No
    //
    //   Target.BaseClass.In-Package = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Java-Identifier </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
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
  public void writeTest_2()
    {
    // properties = baseClass,file,mavenDir

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Java-Identifier
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Default
    //
    //   Source.Operation = Default
    //
    //   Target.Test-Name.Defined = No
    //
    //   Target.Test-Name.Value = (not applicable)
    //
    //   Target.Output-Stream.Defined = No
    //
    //   Target.Output-File.Defined = Yes
    //
    //   Target.Output-File.Path = Absolute
    //
    //   Target.Output-File.Dir.Exists = Yes
    //
    //   Target.Output-File.Dir.In-Maven-Project = Yes
    //
    //   Target.Output-File.Name = Test-Class-Name
    //
    //   Target.Output-Dir.Defined = No
    //
    //   Target.Output-Dir.Path = (not applicable)
    //
    //   Target.Output-Dir.Dir.Exists = (not applicable)
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Package = Default
    //
    //   Target.BaseClass.Defined = Class
    //
    //   Target.BaseClass.In-Package = Yes
    
    // When...

    // Then...
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
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
  public void writeTest_3()
    {
    // properties = baseClass,dir,file,mavenDir,op,path,testName

    // Given...
    //
    //   Source.Request-Cases.Api-Name = File-Name
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Defined
    //
    //   Source.Operation = Defined
    //
    //   Target.Test-Name.Defined = Yes
    //
    //   Target.Test-Name.Value = File-Name
    //
    //   Target.Output-Stream.Defined = Yes
    //
    //   Target.Output-File.Defined = Yes
    //
    //   Target.Output-File.Path = Relative
    //
    //   Target.Output-File.Dir.Exists = No
    //
    //   Target.Output-File.Dir.In-Maven-Project = No
    //
    //   Target.Output-File.Name = Java-Identifier
    //
    //   Target.Output-Dir.Defined = Yes
    //
    //   Target.Output-Dir.Path = Relative
    //
    //   Target.Output-Dir.Dir.Exists = Yes
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = Yes
    //
    //   Target.Package = Defined
    //
    //   Target.BaseClass.Defined = Name
    //
    //   Target.BaseClass.In-Package = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
   * <TR><TD> Source.Operation </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.Test-Name.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Test-Name.Value </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> Target.Output-Stream.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-File.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.Exists </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Dir.In-Maven-Project </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-File.Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Target.Output-Dir.Defined </TD> <TD> Yes </TD> </TR>
   * <TR><TD> Target.Output-Dir.Path </TD> <TD> Absolute </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.Exists </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Output-Dir.Dir.In-Maven-Project </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.Package </TD> <TD> Default </TD> </TR>
   * <TR><TD> Target.BaseClass.Defined </TD> <TD> No </TD> </TR>
   * <TR><TD> Target.BaseClass.In-Package </TD> <TD> (not applicable) </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void writeTest_4()
    {
    // properties = dir,file,mavenDir

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Test-Class-Name
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Default
    //
    //   Source.Operation = Default
    //
    //   Target.Test-Name.Defined = No
    //
    //   Target.Test-Name.Value = (not applicable)
    //
    //   Target.Output-Stream.Defined = No
    //
    //   Target.Output-File.Defined = Yes
    //
    //   Target.Output-File.Path = Absolute
    //
    //   Target.Output-File.Dir.Exists = Yes
    //
    //   Target.Output-File.Dir.In-Maven-Project = Yes
    //
    //   Target.Output-File.Name = File-Name
    //
    //   Target.Output-Dir.Defined = Yes
    //
    //   Target.Output-Dir.Path = Absolute
    //
    //   Target.Output-Dir.Dir.Exists = No
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = No
    //
    //   Target.Package = Default
    //
    //   Target.BaseClass.Defined = No
    //
    //   Target.BaseClass.In-Package = (not applicable)
    
    // When...

    // Then...
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
  public void writeTest_5()
    {
    // properties = baseClass,file,op,path,testName

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Any-Name
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Defined
    //
    //   Source.Operation = Defined
    //
    //   Target.Test-Name.Defined = Yes
    //
    //   Target.Test-Name.Value = Test-Class-Name
    //
    //   Target.Output-Stream.Defined = Yes
    //
    //   Target.Output-File.Defined = Yes
    //
    //   Target.Output-File.Path = Relative
    //
    //   Target.Output-File.Dir.Exists = No
    //
    //   Target.Output-File.Dir.In-Maven-Project = No
    //
    //   Target.Output-File.Name = Test-Class-Name
    //
    //   Target.Output-Dir.Defined = No
    //
    //   Target.Output-Dir.Path = (not applicable)
    //
    //   Target.Output-Dir.Dir.Exists = (not applicable)
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Package = Defined
    //
    //   Target.BaseClass.Defined = Class
    //
    //   Target.BaseClass.In-Package = Yes
    
    // When...

    // Then...
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
  public void writeTest_6()
    {
    // properties = baseClass

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Java-Identifier
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Default
    //
    //   Source.Operation = Default
    //
    //   Target.Test-Name.Defined = No
    //
    //   Target.Test-Name.Value = (not applicable)
    //
    //   Target.Output-Stream.Defined = Yes
    //
    //   Target.Output-File.Defined = No
    //
    //   Target.Output-File.Path = (not applicable)
    //
    //   Target.Output-File.Dir.Exists = (not applicable)
    //
    //   Target.Output-File.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Output-File.Name = (not applicable)
    //
    //   Target.Output-Dir.Defined = No
    //
    //   Target.Output-Dir.Path = (not applicable)
    //
    //   Target.Output-Dir.Dir.Exists = (not applicable)
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Package = Defined
    //
    //   Target.BaseClass.Defined = Name
    //
    //   Target.BaseClass.In-Package = No
    
    // When...

    // Then...
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. writeTest (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> File-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Defined </TD> </TR>
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
  public void writeTest_7()
    {
    // properties = dir,mavenDir,op,path,testName

    // Given...
    //
    //   Source.Request-Cases.Api-Name = File-Name
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Defined
    //
    //   Source.Operation = Defined
    //
    //   Target.Test-Name.Defined = Yes
    //
    //   Target.Test-Name.Value = Java-Identifier
    //
    //   Target.Output-Stream.Defined = No
    //
    //   Target.Output-File.Defined = No
    //
    //   Target.Output-File.Path = (not applicable)
    //
    //   Target.Output-File.Dir.Exists = (not applicable)
    //
    //   Target.Output-File.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Output-File.Name = (not applicable)
    //
    //   Target.Output-Dir.Defined = Yes
    //
    //   Target.Output-Dir.Path = Relative
    //
    //   Target.Output-Dir.Dir.Exists = Yes
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = Yes
    //
    //   Target.Package = Default
    //
    //   Target.BaseClass.Defined = No
    //
    //   Target.BaseClass.In-Package = (not applicable)
    
    // When...

    // Then...
    }

  /**
   * Tests {@link JUnitTestWriter#writeTest writeTest()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. writeTest (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> Source.Request-Cases.Api-Name </TD> <TD> Test-Class-Name </TD> </TR>
   * <TR><TD> Source.Request-Cases.Count </TD> <TD> Some </TD> </TR>
   * <TR><TD> Source.Path </TD> <TD> Default </TD> </TR>
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
  public void writeTest_8()
    {
    // properties = baseClass,file

    // Given...
    //
    //   Source.Request-Cases.Api-Name = Test-Class-Name
    //
    //   Source.Request-Cases.Count = Some
    //
    //   Source.Path = Default
    //
    //   Source.Operation = Default
    //
    //   Target.Test-Name.Defined = No
    //
    //   Target.Test-Name.Value = (not applicable)
    //
    //   Target.Output-Stream.Defined = No
    //
    //   Target.Output-File.Defined = Yes
    //
    //   Target.Output-File.Path = Absolute
    //
    //   Target.Output-File.Dir.Exists = Yes
    //
    //   Target.Output-File.Dir.In-Maven-Project = No
    //
    //   Target.Output-File.Name = Java-Identifier
    //
    //   Target.Output-Dir.Defined = No
    //
    //   Target.Output-Dir.Path = (not applicable)
    //
    //   Target.Output-Dir.Dir.Exists = (not applicable)
    //
    //   Target.Output-Dir.Dir.In-Maven-Project = (not applicable)
    //
    //   Target.Package = Undefined
    //
    //   Target.BaseClass.Defined = Class
    //
    //   Target.BaseClass.In-Package = Yes
    
    // When...

    // Then...
    }

  /**
   * Verifies that the test writer results for the given request test definition match expectations.
   */
  protected void verifyTest( String testDefName, String testWriterResults)
    {
    File expectedResults = getExpectedTestResults( testDefName);
    if( acceptAsExpected())
      {
      updateTestResults( expectedResults.getName(), testWriterResults);
      }
    else
      {
      String expected;
      try
        {
        expected = FileUtils.readFileToString( expectedResults, "UTF-8");
        }
      catch( Exception e)
        {
        throw new RuntimeException( String.format( "Can't read expected results for testDef=%s", testDefName), e);
        }

      assertTestResultsEqual( testDefName, testWriterResults, expected);
      }
    }

  /**
   * Reports a failure if the actual test results are not equal to the expected results.
   */
  protected void assertTestResultsEqual( String testDefName, String actual, String expected)
    {
    int diffStart = StringUtils.indexOfDifference( actual, expected);
    if( diffStart >= 0)
      {
      int sampleSize = 16;
      int sampleStart = Math.max( 0, diffStart - sampleSize/2);
      int actualSampleEnd = Math.min( diffStart + sampleSize/2, actual.length());
      int expectedSampleEnd = Math.min( diffStart + sampleSize/2, expected.length());

      fail(
        String.format(
          "Unexpected results for %s starting at index=%s -- expected '%s%s%s' but was '%s%s%s'",
          testDefName,
          diffStart,
          sampleStart == 0? "" : "...",
          expected.substring( sampleStart, expectedSampleEnd).replaceAll( "\\n", "\\\\n"),
          expectedSampleEnd == expected.length()? "" : "...",
          sampleStart == 0? "" : "...",
          actual.substring( sampleStart, actualSampleEnd).replaceAll( "\\n", "\\\\n"),
          actualSampleEnd == actual.length()? "" : "..."));
      }
    }

  /**
   * Updates expected test writer results.
   */
  private void updateTestResults( String expectedResultsName, String actualResults)
    {
    try
      {
      File expectedResults = new File( saveExpectedDir_, expectedResultsName);
      FileUtils.write( expectedResults, actualResults, "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( String.format( "Can't update expectedResults=%s", expectedResultsName), e);
      }
    }

  /**
   * Returns the {@link RequestTestDef} object represented by the given document resource.
   */
  protected RequestTestDef requestTestDefFor( String testDefName)
    {
    InputStream document = getClass().getResourceAsStream( String.format( "%s-Request-Cases.json", testDefName));
    assertThat( "Request cases for resource=" + testDefName, document, is( notNullValue()));
    
    try( RequestTestDefReader reader = new RequestTestDefReader( document))
      {
      return reader.getRequestTestDef();
      }
    }

  /**
   * Verifies that the test writer results for the given request test definition match expectations.
   */
  protected File getExpectedTestResults( String testDefName)
    {
    return new File( getResourceDir(), testDefName + "-Expected-Test.java");
    }

  /**
   * Runs the given Runnable and returns the results printed to standard output
   */
  private String toStdOut( Runnable runnable)
    {
    try
      {
      PrintStream prevOut = System.out;
      PrintStream newOut = null;
      ByteArrayOutputStream newOutBytes = null;

      try
        {
        newOutBytes = new ByteArrayOutputStream();
        newOut = new PrintStream( newOutBytes, true, "UTF-8");
        System.setOut( newOut);

        runnable.run();
        }
      finally
        {
        IOUtils.closeQuietly( newOut);
        System.setOut( prevOut);
        }

      return new String( newOutBytes.toByteArray(), "UTF-8");
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get results from standard output", e);
      }
    }

  /**
   * Returns the location of resource files.
   */
  protected File getResourceDir()
    {
    try
      {
      return new File( getClass().getResource( ".").toURI().getPath());
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't get resource directory path", e);
      }
    }

  /**
   * Returns true if all generated results are automatically accepted.
   */
  private boolean acceptAsExpected()
    {
    return saveExpectedDir_ != null;
    }

  private final File saveExpectedDir_ =
    Optional.ofNullable( StringUtils.trimToNull( System.getProperty( "saveExpectedTo")))
    .map( path -> new File( path))
    .orElse( null);

  /**
   * A mock {@link TestCaseWriter}
   */
  public class MockTestCaseWriter implements TestCaseWriter
    {
    /**
     * Writes the dependencies for target test cases to the given stream.
     */
    public void writeDependencies( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case dependencies");
      }

    /**
     * Writes the declarations for target test cases to the given stream.
     */
    public void writeDeclarations( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case declarations");
      }
  
    /**
     * Writes a target test case to the given stream.
     */
    public void writeTestCase( String testName, RequestCase requestCase, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Given...");
      targetWriter.println( "// When...");
      targetWriter.println( "// Then...");
      }

    /**
     * Writes the closing for target test cases the given stream.
     */
    public void writeClosing( String testName, IndentedWriter targetWriter)
      {
      targetWriter.println( "// Test case closing");
      }
    }
  }
