//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.openapi.io.TcasesOpenApiIO;
import org.cornutum.tcases.openapi.resolver.*;
import org.cornutum.tcases.openapi.restassured.RestAssuredTestCaseWriter;
import org.cornutum.tcases.openapi.testwriter.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toSet;

/**
 * Generates executable test code for API servers, based on an OpenAPI v3 compliant API spec.
 */
public class ApiTestCommand
  {
  /**
   * Represents a set of command line options.
   *
   * Command line arguments have the following form.
   * <P/>
   * <BLOCKQUOTE>
   * <CODE>
   * <TABLE cellspacing="0" cellpadding="8">
   * <TR valign="top">
   * <TD colspan="3">
   * <NOBR>
   * [<I>option</I>...] [<I>apiSpec</I>]
   * </NOBR>
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD colspan="3">
   * where each <I>option</I> is one of the following:
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-t testType </NOBR>
   * </TD>
   * <TD>
   * Defines the test framework used to run API tests. Valid values are <CODE>junit</CODE>, <CODE>testng</CODE>, or <CODE>moco</CODE>.
   * If omitted, the default is <CODE>junit</CODE>.
   * <P/>
   * Use <CODE>moco</CODE> to generate a JUnit test that sends requests to a <A href="https://github.com/dreamhead/moco">Moco stub server</A>.
   * To define the Moco server test configuration, use the <CODE>-M</CODE> option.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-e execType </NOBR>
   * </TD>
   * <TD>
   * Defines the request execution interface used to run API tests. Valid values are <CODE>restassured</CODE>.
   * If omitted, the default is <CODE>restassured</CODE>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-name testName </NOBR>
   * </TD>
   * <TD>
   * Defines the name of the test class that is generated. This can be either a fully-qualified class name
   * or a simple class name. If omitted, the default is based on the title of the <CODE>apiSpec</CODE>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-p testPackage </NOBR>
   * </TD>
   * <TD>
   * Defines the package for the test class that is generated. This can be omitted if the <CODE>testName</CODE>
   * is a fully-qualified class name or if the package can be determined from the <CODE>outDir</CODE>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-b baseClass </NOBR>
   * </TD>
   * <TD>
   * If defined, specifies a base class for the generated test class. This can be a fully-qualified class name
   * or a simple class name, if the <CODE>baseClass</CODE> belongs to the same package as the generated test class.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-f <I>outFile</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-f</I> is defined, output is written to the specified <I>outFile</I>, relative to the given <I>outDir</I>.
   * If omitted, the default <I>outFile</I> is derived from the <I>testName</I>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-o <I>outDir</I> </NOBR>
   * </TD>
   * <TD>
   * If <I>-o</I> is defined, output is written to the specified directory.
   * If omitted, the default <I>outDir</I> is the directory containing the <I>apiSpec</I> or,
   * if reading from standard input, the current working directory. If an output path cannot be
   * derived, output is written to standard output.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-M mocoTestConfig </NOBR>
   * </TD>
   * <TD>
   * When the <CODE>testType</CODE> is <CODE>moco</CODE>, specifies the Moco server test configuration file.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-P paths </NOBR>
   * </TD>
   * <TD>
   * If defined, tests are generated only for the specified API resource paths. <CODE>paths</CODE> must be a comma-separated list
   * of resource paths defined in the <CODE>apiSpec</CODE>.
   * If omitted, tests are generated for all resource paths.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-O operations </NOBR>
   * </TD>
   * <TD>
   * If defined, tests are generated only for the specified HTTP methods. <CODE>operations</CODE> must be a comma-separated list
   * of path operations defined in the <CODE>apiSpec</CODE>.
   * If omitted, tests are generated for all operations.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-T <I>contentType</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the content type of the OpenApi specification. The <I>contentType</I> must be one of "json", "yaml", or "yml".
   * If omitted, the default content type is derived from the <I>apiSpec</I> name. If the <I>apiSpec</I> is read from standard
   * input or does not have a recognized extension, the default content type is "json".
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-c M[,R] </NOBR>
   * </TD>
   * <TD>
   * Defines how input modelling and request case resolution conditions are reported. Both <CODE>M</CODE> (for modelling conditions) and <CODE>R</CODE> (for
   * resolution conditions) must be one of <CODE>log</CODE>, <CODE>fail</CODE>, or <CODE>ignore</CODE>.
   * If <CODE>log</CODE> is specified, conditions are reported using log messages.
   * If <CODE>fail</CODE> is specified, any condition will cause an exception. If <CODE>ignore</CODE> is specified, all conditions
   * are silently ignored. If <CODE>R</CODE> is omitted, the default is <CODE>log</CODE>. If <I>-c</I> is omitted, the default is <CODE>log,log</CODE>.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-R </NOBR>
   * </TD>
   * <TD>
   * If specified, tests will be generated assuming that the API will strictly enforce exclusion of "readOnly"
   * properties from request parameters. If omitted, no strict enforcement is assumed.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-r <I>seed</I> </NOBR>
   * </TD>
   * <TD>
   * If defined, use the given random number seed to generate request test case input values. 
   * If omitted, the default random number seed is derived from the <I>apiSpec</I> name.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-m <I>maxTries</I> </NOBR>
   * </TD>
   * <TD>
   * Defines the maximum attempts made to resolve a request test case input value before reporting failure.
   * If omitted, the default value is 10000.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR>-v </NOBR>
   * </TD>
   * <TD>
   * Prints the current command version identifier to standard output.
   * </TD>
   * </TR>
   *
   * <TR valign="top">
   * <TD>
   * &nbsp;
   * </TD>
   * <TD>
   * <NOBR><I>apiSpec</I> </NOBR>
   * </TD>
   * <TD>
   * An OpenAPI v3 API spec is read from the given <I>apiSpec</I> file. If omitted, the API spec is
   * read from standard input. If no <I>outFile</I> is specified, output is written to a default file
   * derived from the <I>apiSpec</I> or, if no <I>apiSpec</I> is given, to standard output.
   * </TD>
   * </TR>
   *
   * </TABLE>
   * </CODE>
   * </BLOCKQUOTE>
   */
  public static class Options implements TestTargetFactory, TestWriterFactory, TestCaseWriterFactory
    {
    public enum TestType { JUNIT, TESTNG, MOCO };

    public enum ExecType { RESTASSURED };
    
    /**
     * Creates a new Options object.
     */
    public Options()
      {
      setWorkingDir( null);
      setTestType( TestType.JUNIT);
      setExecType( ExecType.RESTASSURED);
      setModelOptions( new ModelOptions());
      setResolverContext( new ResolverContext( new Random()));
      }

    /**
     * Creates a new Options object.
     */
    public Options( String[] args)
      {
      this();

      int i;

      // Handle options
      for( i = 0; i < args.length && args[i].charAt(0) == '-'; i = handleOption( args, i));

      // Handle additional arguments.
      handleArgs( args, i);
      }

    /**
     * Handles the i'th option and return the index of the next argument.
     */
    protected int handleOption( String[] args, int i)
      {
      String arg = args[i];

      if( arg.equals( "-t"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setTestType( args[i]);
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid test type", e);
          }
        }

      else if( arg.equals( "-e"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setExecType( args[i]);
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid exec type", e);
          }
        }

      else if( arg.equals( "-n"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setTestName( args[i]);
        }

      else if( arg.equals( "-p"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setTestPackage( args[i]);
        }

      else if( arg.equals( "-b"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setBaseClass( args[i]);
        }

      else if( arg.equals( "-f"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setOutFile( new File( args[i]));
        }

      else if( arg.equals( "-o"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setOutDir( new File( args[i]));
        }

      else if( arg.equals( "-M"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setMocoTestConfig( new File( args[i]));
        }

      else if( arg.equals( "-P"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setPaths( Arrays.asList( args[i].split( " *, *")));
        }

      else if( arg.equals( "-O"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setOperations( Arrays.asList( args[i].split( " *, *")));
        }

      else if( arg.equals( "-c"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        setConditionNotifiers( args[i]);
        }

      else if( arg.equals( "-R"))
        {
        setReadOnlyEnforced( true);
        }

      else if( arg.equals( "-r"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setRandomSeed( Long.valueOf( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid random seed", e);
          }
        }

      else if( arg.equals( "-m"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setMaxTries( Integer.parseInt( args[i]));
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid max tries", e);
          }
        }

      else if( arg.equals( "-T"))
        {
        i++;
        if( i >= args.length)
          {
          throwUsageException();
          }
        try
          {
          setContentType( args[i]);
          }
        catch( Exception e)
          {
          throwUsageException( "Invalid content type", e);
          }
        }

      else if( arg.equals( "-v"))
        {
        setShowVersion( true);
        }

      else
        {
        throwUsageException();
        }

      return i + 1;
      }

    /**
     * Handles the non-option arguments i, i+1, ...
     */
    protected void handleArgs( String[] args, int i)
      {
      int nargs = args.length - i;

      if( nargs > 1)
        {
        throwUsageException();
        }

      if( nargs > 0)
        {
        setApiSpec( new File( args[i]));
        }
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException()
      {
      throwUsageException( null, null);
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException( String detail)
      {
      throwUsageException( detail, null);
      }

    /**
     * Throws a RuntimeException reporting a command line error.
     */
    protected void throwUsageException( String detail, Exception cause)
      {
      throw getUsageException( detail, cause);
      }

    /**
     * Returns a RuntimeException reporting a command line error.
     */
    protected RuntimeException getUsageException( String detail, Exception cause)
      {
      if( detail != null)
        {
        cause = new RuntimeException( detail, cause);
        }

      return
        new IllegalArgumentException
        ( "Usage: "
          + ApiTestCommand.class.getSimpleName()
          + " [-v]"
          + " [-t testType]"
          + " [-e execType]"
          + " [-n testName]"
          + " [-p testPackage]"
          + " [-b baseClass]"
          + " [-f outFile]"
          + " [-o outDir]"
          + " [-M mocoTestConfig]"
          + " [-P paths]"
          + " [-O operations]"
          + " [-T contentType]"
          + " [-c condRep[,condRep]]"
          + " [-R]"
          + " [-r seed]"
          + " [-m maxTries]"
          + " [apiSpec]",
          cause);
      }

    /**
     * Changes the test framework used to run API tests.
     */
    public void setTestType( TestType testType)
      {
      testType_ = testType;
      }

    /**
     * Changes the test framework used to run API tests.
     */
    public void setTestType( String testType)
      {
      setTestType( TestType.valueOf( String.valueOf( testType).toUpperCase()));
      }

    /**
     * Returns the test framework used to run API tests.
     */
    public TestType getTestType()
      {
      return testType_;
      }

    /**
     * Changes the request execution intervce used to run API tests.
     */
    public void setExecType( ExecType execType)
      {
      execType_ = execType;
      }

    /**
     * Changes the request execution intervce used to run API tests.
     */
    public void setExecType( String execType)
      {
      setExecType( ExecType.valueOf( String.valueOf( execType).toUpperCase()));
      }

    /**
     * Returns the request execution intervce used to run API tests.
     */
    public ExecType getExecType()
      {
      return execType_;
      }

    /**
     * Changes the name of the test class that is generated.
     */
    public void setTestName( String testName)
      {
      List<String> fqn =
        Optional.ofNullable( StringUtils.trimToNull( testName))
        .map( name -> Arrays.asList( name.split( "\\.")))
        .orElse( null);

      Optional.ofNullable( fqn)
        .filter( path -> path.size() > 1)
        .ifPresent( path -> setTestPackage( path.subList( 0, path.size() - 1).stream().collect( joining( "."))));
      
      testName_ =
        Optional.ofNullable( fqn)
        .map( path -> path.get( path.size() - 1))
        .orElse( null);
      }

    /**
     * Returns the name of the test class that is generated.
     */
    public String getTestName()
      {
      return testName_;
      }

    /**
     * Changes the package of the test class that is generated.
     */
    public void setTestPackage( String testPackage)
      {
      testPackage_ = testPackage;
      }

    /**
     * Returns the package of the test class that is generated.
     */
    public String getTestPackage()
      {
      return testPackage_;
      }

    /**
     * Changes the base class for the test class that is generated.
     */
    public void setBaseClass( String baseClass)
      {
      baseClass_ = baseClass;
      }

    /**
     * Returns the base class for the test class that is generated.
     */
    public String getBaseClass()
      {
      return baseClass_;
      }

    /**
     * Changes the output directory for command output.
     */
    public void setOutDir( File outDir)
      {
      outDir_ = outDir;
      }

    /**
     * Returns the output directory for command output.
     */
    public File getOutDir()
      {
      return outDir_;
      }

    /**
     * Changes the output file for command output.
     */
    public void setOutFile( File outFile)
      {
      outFile_ = outFile;
      }

    /**
     * Returns the output file for command output.
     */
    public File getOutFile()
      {
      return outFile_;
      }

    /**
     * Changes the Moco server test configuration file
     */
    public void setMocoTestConfig( File mocoTestConfig)
      {
      mocoTestConfig_ = mocoTestConfig;
      }

    /**
     * Returns the Moco server test configuration file
     */
    public File getMocoTestConfig()
      {
      return mocoTestConfig_;
      }

    /**
     * Changes request paths for which tests are generated.
     */
    public void setPaths( Iterable<String> paths)
      {
      paths_ =
        Optional.ofNullable( toStream( paths))
        .map( s -> s.collect( toSet()))
        .orElse( null);
      }

    /**
     * Returns request paths for which tests are generated.
     */
    public Set<String> getPaths()
      {
      return paths_;
      }

    /**
     * Changes request path operations for which tests are generated.
     */
    public void setOperations( Iterable<String> operations)
      {
      operations_ =
        Optional.ofNullable( toStream( operations))
        .map( s -> s.collect( toSet()))
        .orElse( null);
      }

    /**
     * Returns request path operations for which tests are generated.
     */
    public Set<String> getOperations()
      {
      return operations_;
      }

    /**
     * Changes the OpenApi spec file content type.
     */
    public void setContentType( String option)
      {
      String contentType =
        Optional.ofNullable( option)
        .map( String::toLowerCase)
        .filter( type -> "json".equals( type) || "yml".equals( type) || "yaml".equals( type))
        .orElse( null);
      
      if( option != null && contentType == null)
        {
        throw new IllegalArgumentException( String.format( "'%s' is not a valid content type", option));
        }

      contentType_ = contentType;
      }

    /**
     * Returns the OpenApi spec file content type.
     */
    public String getContentType()
      {
      return contentType_;
      }

    /**
     * Changes the input modelling options.
     */
    public void setModelOptions( ModelOptions modelOptions)
      {
      modelOptions_ = modelOptions;
      }

    /**
     * Returns the input modelling options.
     */
    public ModelOptions getModelOptions()
      {
      return modelOptions_;
      }

    /**
     * Changes the request case resolution options.
     */
    public void setResolverContext( ResolverContext resolverContext)
      {
      resolverContext_ = resolverContext;
      }

    /**
     * Returns the request case resolution options.
     */
    public ResolverContext getResolverContext()
      {
      return resolverContext_;
      }

    /**
     * Changes the random number generator seed for request case resolution.
     */
    public void setRandomSeed( Long seed)
      {
      randomSeed_ = seed;
      if( seed != null)
        {
        getResolverContext().setRandom( new Random( seed));
        }
      }

    /**
     * Returns the random number generator seed for request case resolution.
     */
    public Long getRandomSeed()
      {
      return randomSeed_;
      }

    /**
     * Returns the default random number generator seed for request case resolution.
     */
    public Long getDefaultRandomSeed()
      {
      return
        Optional.ofNullable( getApiSpec())
        .map( spec -> (long) spec.getName().hashCode())
        .orElse( new Random().nextLong());
      }

    /**
     * Changes the maximum attempts made to resolve a request test case input value before reporting failure..
     */
    public void setMaxTries( int maxTries)
      {
      getResolverContext().setMaxTries( maxTries);
      }

    /**
     * Returns the maximum attempts made to resolve a request test case input value before reporting failure..
     */
    public int getMaxTries()
      {
      return getResolverContext().getMaxTries();
      }

    /**
     * Changes condition notifiers for input modelling and request case resolution conditions.
     */
    public void setConditionNotifiers( String notifierList)
      {
      String[] notifiers = notifierList.split( ",", -1);

      String modelNotifier = notifiers.length > 0? StringUtils.trimToNull( notifiers[0]) : null;
      setOnModellingCondition( modelNotifier);

      String resolveNotifier = notifiers.length > 1? StringUtils.trimToNull( notifiers[1]) : null;
      setOnResolverCondition( resolveNotifier);
      }

    /**
     * Changes the input modelling condition notifier.
     */
    public void setOnModellingCondition( String notifier)
      {
      getModelOptions().setConditionNotifier(
        Optional.ofNullable(
          notifier == null || "log".equals( notifier)?
          ModelConditionNotifier.log() :

          "fail".equals( notifier)?
          ModelConditionNotifier.fail() :

          "ignore".equals( notifier)?
          ModelConditionNotifier.ignore() :

          null)

        .orElseThrow( () -> getUsageException( "Unknown condition notifier: " + notifier, null)));
      }

    /**
     * Changes the request case resolution condition notifier.
     */
    public void setOnResolverCondition( String notifier)
      {
      getResolverContext().setNotifier(
        Optional.ofNullable(
          notifier == null || "log".equals( notifier)?
          ResolverConditionNotifier.log() :

          "fail".equals( notifier)?
          ResolverConditionNotifier.fail() :

          "ignore".equals( notifier)?
          ResolverConditionNotifier.ignore() :

          null)

        .orElseThrow( () -> getUsageException( "Unknown condition notifier: " + notifier, null)));
      }

    /**
     * Changes if "readOnly" properties are strictly enforced.
     */
    public void setReadOnlyEnforced( boolean enforced)
      {
      getModelOptions().setReadOnlyEnforced( enforced);
      }

    /**
     * Changes the Open API v3 API spec file
     */
    public void setApiSpec( File apiSpec)
      {
      apiSpec_ = apiSpec;
      }

    /**
     * Returns the Open API v3 API spec file
     */
    public File getApiSpec()
      {
      return apiSpec_;
      }

    /**
     * Changes the current working directory used to complete relative path names.
     */
    public void setWorkingDir( File workingDir)
      {
      workingDir_ =
        workingDir == null
        ? new File( ".")
        : workingDir;
      }

    /**
     * Returns the current working directory used to complete relative path names.
     */
    public File getWorkingDir()
      {
      return workingDir_;
      }

    /**
     * Changes if the current version should be shown.
     */
    public void setShowVersion( boolean showVersion)
      {
      showVersion_ = showVersion;
      }

    /**
     * Returns if the current version should be shown.
     */
    public boolean showVersion()
      {
      return showVersion_;
      }

    /**
     * Returns the {@link TestSource} defined by these options.
     */
    public TestSource getTestSource( RequestTestDef testDef)
      {
      TestSource testSource = new TestSource( testDef);
      testSource.setPaths( getPaths());
      testSource.setOperations( getOperations());
      return testSource;
      }

    /**
     * Returns the {@link TestTarget} defined by these options.
     */
    public TestTarget getTestTarget()
      {
      return createTestTarget();
      }

    /**
     * Returns the {@link TestWriter} defined by these options.
     */
    public TestWriter<?,?> getTestWriter( TestCaseWriter testCaseWriter)
      {
      return createTestWriter( testCaseWriter);
      }

    /**
     * Returns the {@link TestCaseWriter} defined by these options.
     */
    public TestCaseWriter getTestCaseWriter()
      {
      return createTestCaseWriter();
      }

    /**
     * Creates a new {@link TestTarget} instance.
     */
    public TestTarget createTestTarget()
      {
      return
        JavaTestTarget.builder()
        .named( getTestName())
        .inPackage( getTestPackage())
        .extending( getBaseClass())
        .toFile( getOutFile())
        .inDir( getOutDir())
        .build();
      }
    
    /**
     * Creates a new {@link TestWriter} instance.
     */
    public TestWriter<?,?> createTestWriter( TestCaseWriter testCaseWriter)
      {
      TestWriter<?,?> testWriter;
      
      switch( getTestType())
        {
        case JUNIT:
          {
          testWriter = new JUnitTestWriter( testCaseWriter);
          break;
          }
        case TESTNG:
          {
          testWriter = new TestNgTestWriter( testCaseWriter);
          break;
          }
        case MOCO:
          {
          testWriter = null;
          break;
          }
        default:
          {
          throw new IllegalArgumentException( String.format( "%s is not a valid test type", getTestType()));
          }
        }
      
      return testWriter;
      }
    
    /**
     * Creates a new {@link TestCaseWriter} instance.
     */
    public TestCaseWriter createTestCaseWriter()
      {
      return new RestAssuredTestCaseWriter();
      }

    /**
     * Returns a new Options builder.
     */
    public static Builder builder()
      {
      return new Builder();
      }

    public String toString()
      {
      StringBuilder builder = new StringBuilder();

      builder.append( " -t ").append( getTestType());
      builder.append( " -e ").append( getExecType());
      Optional.ofNullable( getTestName()).ifPresent( name -> builder.append( " -n ").append( name));
      Optional.ofNullable( getTestPackage()).ifPresent( pkg -> builder.append( " -p ").append( pkg));
      Optional.ofNullable( getBaseClass()).ifPresent( base -> builder.append( " -b ").append( base));
      Optional.ofNullable( getOutFile()).ifPresent( file -> builder.append( " -f ").append( file.getPath()));
      Optional.ofNullable( getOutDir()).ifPresent( dir -> builder.append( " -o ").append( dir.getPath()));   
      Optional.ofNullable( getMocoTestConfig()).ifPresent( moco -> builder.append( " -M ").append( moco.getPath()));   
      Optional.ofNullable( getPaths()).ifPresent( paths -> builder.append( " -P ").append( paths.stream().collect( joining( ","))));   
      Optional.ofNullable( getOperations()).ifPresent( operations -> builder.append( " -O ").append( operations.stream().collect( joining( ","))));
      builder.append( " -c ").append( String.format( "%s,%s", getModelOptions().getConditionNotifier(), getResolverContext().getNotifier()));
      Optional.of( getModelOptions()).filter( ModelOptions::isReadOnlyEnforced).ifPresent( o -> builder.append( " -R")); 
      builder.append( " -m ").append( getMaxTries());
      Optional.ofNullable( getRandomSeed()).ifPresent( seed -> builder.append( " -r ").append( seed));
      Optional.ofNullable( getContentType()).ifPresent( content -> builder.append( " -T ").append( content));

      if( showVersion())
        {
        builder.append( " -v");
        }

      return builder.toString();
      }

    private File apiSpec_;
    private TestType testType_;
    private ExecType execType_;
    private String testName_;
    private String testPackage_;
    private String baseClass_;
    private File outDir_;
    private File outFile_;
    private File mocoTestConfig_;
    private Set<String> paths_;
    private Set<String> operations_;
    private String contentType_;
    private ModelOptions modelOptions_;
    private ResolverContext resolverContext_;
    private File workingDir_;
    private boolean showVersion_;
    private Long randomSeed_;

    public static class Builder
      {
      public Builder()
        {
        options_ = new Options();
        }

      public Builder apiSpec( File apiSpec)
        {
        options_.setApiSpec( apiSpec);
        return this;
        }

      public Builder testType( TestType testType)
        {
        options_.setTestType( testType);
        return this;
        }

      public Builder execType( ExecType execType)
        {
        options_.setExecType( execType);
        return this;
        }

      public Builder testName( String testName)
        {
        options_.setTestName( testName);
        return this;
        }

      public Builder testPackage( String testPackage)
        {
        options_.setTestPackage( testPackage);
        return this;
        }

      public Builder baseClass( String baseClass)
        {
        options_.setBaseClass( baseClass);
        return this;
        }

      public Builder outFile( File outFile)
        {
        options_.setOutFile( outFile);
        return this;
        }

      public Builder outDir( File outDir)
        {
        options_.setOutDir( outDir);
        return this;
        }

      public Builder mocoTestConfig( File mocoTestConfig)
        {
        options_.setMocoTestConfig( mocoTestConfig);
        return this;
        }

      public Builder paths( String... paths)
        {
        options_.setPaths( Arrays.asList( paths));
        return this;
        }

      public Builder operations( String... operations)
        {
        options_.setOperations( Arrays.asList( operations));
        return this;
        }

      public Builder contentType( String type)
        {
        options_.setContentType( type);
        return this;
        }

      public Builder onModellingCondition( String notifier)
        {
        options_.setOnModellingCondition( notifier);
        return this;
        }

      public Builder onResolverCondition( String notifier)
        {
        options_.setOnResolverCondition( notifier);
        return this;
        }

      public Builder enforceReadOnly()
        {
        options_.setReadOnlyEnforced( true);
        return this;
        }

      public Builder random( Long seed)
        {
        options_.setRandomSeed( seed);
        return this;
        }

      public Builder maxTries( int maxTries)
        {
        options_.setMaxTries( maxTries);
        return this;
        }

      public Options build()
        {
        return options_;
        }
      
      private Options options_;
      }
    }
  
  /**
   * Creates a new ApiTestCommand object.
   */
  private ApiTestCommand()
    {
    // Static methods only
    }

  /**
   * Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec,
   * using the given {@link Options command line options}.
   */
  public static void main( String[] args)
    {
    int exitCode = 0;
    try
      {
      run( new Options( args));
      }
    catch( Exception e)
      {
      exitCode = 1;
      e.printStackTrace( System.err);
      }
    finally
      {
      System.exit( exitCode);
      }
    }

  /**
   * Generates input models and test models for API clients and servers, based on an OpenAPI v3 compliant API spec,
   * using the given {@link Options command line options}.
   */
  public static void run( Options options) throws Exception
    {
    if( options.showVersion())
      {
      System.out.println( getVersion());
      return;
      }
    logger_.info( "{}", getVersion());

    // Identify the API spec file
    File apiSpecFile = options.getApiSpec();
    if( apiSpecFile != null && !apiSpecFile.isAbsolute())
      {
      apiSpecFile = new File( options.getWorkingDir(), apiSpecFile.getPath());
      }

    File inputDir =
      apiSpecFile==null
      ? options.getWorkingDir()
      : apiSpecFile.getParentFile();
    
    // Generate requested input definition
    logger_.info( "Reading API spec from {}", Objects.toString( apiSpecFile,  "standard input"));
    SystemInputDef inputDef = TcasesOpenApiIO.getRequestInputModel( apiSpecFile, options.getContentType(), options.getModelOptions());
    if( inputDef == null)
      {
      logger_.warn( "No requests defined");
      }
    else
      {
      if( options.getRandomSeed() == null)
          {
          options.setRandomSeed( options.getDefaultRandomSeed());
          }
      logger_.info( "Generating request test cases using random seed={}", options.getRandomSeed());
      RequestTestDef testDef = RequestCases.getRequestCases( Tcases.getTests( inputDef, null, null), options.getResolverContext());
      TestSource testSource = options.getTestSource( testDef);

      TestCaseWriter testCaseWriter = options.getTestCaseWriter();
      TestWriter<?,?> testWriter = options.getTestWriter( testCaseWriter);

      TestTarget testTarget = options.getTestTarget();
      Optional.ofNullable( getTestFile( testWriter, testSource, testTarget))
        .filter( file -> !file.isAbsolute())
        .ifPresent( file -> testTarget.setDir( inputDir));

      logger_.info( "Writing API test to {}", Objects.toString( getTestFile( testWriter, testSource, testTarget),  "standard output"));
      writeTest( testWriter, testSource, testTarget);
      }
    }

  /**
   * Returns the {@link TestWriter#getTestFile test file} for the given {@link TestWriter}.
   */
  private static File getTestFile( TestWriter<?,?> testWriter, TestSource testSource, TestTarget testTarget)
    {
    Throwable failure = null;
    File testFile = null;

    try
      {
      testFile =
        (File)
        testWriter.getClass()
        .getMethod( "getTestFile", TestSource.class, TestTarget.class)
        .invoke( testWriter, testSource, testTarget);
      }
    catch( InvocationTargetException ite)
      {
      failure = ite.getCause();
      }
    catch( Exception e)
      {
      failure = e;
      }

    if( failure != null)
      {
      throw new TestWriterException( String.format( "Can't get test file for %s", testWriter), failure);
      }

    return testFile;
    }

  /**
   * Applies the given {@link TestWriter} to write a test using the given source and target.
   */
  private static void writeTest( TestWriter<?,?> testWriter, TestSource testSource, TestTarget testTarget)
    {
    Throwable failure = null;

    try
      {
      testWriter.getClass()
        .getMethod( "writeTest", TestSource.class, TestTarget.class)
        .invoke( testWriter, testSource, testTarget);
      }
    catch( InvocationTargetException ite)
      {
      failure = ite.getCause();
      }
    catch( Exception e)
      {
      failure = e;
      }

    if( failure != null)
      {
      throw new TestWriterException( String.format( "%s: Can't write test for %s", testWriter, testSource), failure);
      }
    }

  /**
   * Returns a description of the current version.
   */
  public static String getVersion()
    {
    Properties tcasesProperties = new Properties();
    String tcasesPropertyFileName = "/tcases.properties";
    InputStream tcasesPropertyFile = null;
    try
      {
      tcasesPropertyFile = Tcases.class.getResourceAsStream( tcasesPropertyFileName);
      tcasesProperties.load( tcasesPropertyFile);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read " + tcasesPropertyFileName, e);
      }
    finally
      {
      IOUtils.closeQuietly( tcasesPropertyFile);
      }

    return
      String.format(
        "%s (%s)",
        tcasesProperties.getProperty( "tcases.version"),
        tcasesProperties.getProperty( "tcases.date"));
    }

  private static final Logger logger_ = LoggerFactory.getLogger( ApiTestCommand.class);
  }
