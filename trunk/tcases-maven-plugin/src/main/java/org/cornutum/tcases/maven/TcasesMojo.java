package org.cornutum.tcases.maven;

import org.cornutum.tcases.Tcases;
import org.cornutum.tcases.Tcases.Options;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;

import java.io.File;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Implements the Tcases Maven plugin.
 *
 */
@Mojo(name="tcases",defaultPhase=LifecyclePhase.GENERATE_TEST_RESOURCES)
public class TcasesMojo extends AbstractMojo
  {
  public void execute() throws MojoExecutionException
    {
    try
      {
      // Initialize Tcase log file
      System.setProperty( "tcases.log.file", getLogFile().getAbsolutePath());
      
      // Gather input definition files
      DirectoryScanner inputScanner = new DirectoryScanner();
      if( getInputDefs().isEmpty())
        {
        getInputDefs().add( getInputDef());
        }
      inputScanner.setIncludes( getInputDefs().toArray( new String[0]));
      inputScanner.setBasedir( getInputDir());
      inputScanner.scan();

      // Generate test cases for each input definition file.
      Tcases tcases = new Tcases();
      String[] inputDefs = inputScanner.getIncludedFiles();
      for( int i = 0; i < inputDefs.length; i++)
        {
        // Define input and output paths for this Tcases project.
        String inputFile = inputDefs[i];
        String inputPath = FileUtils.dirname( inputFile);
        File inputDef = new File( getInputDir(), inputFile);
        File inputDir = new File( getInputDir(), inputPath);
        File outDir = new File( getOutDir(), inputPath);

        String projectName = Tcases.getProjectName( inputDef);
        String outFile = getOutFile();
        if( outFile != null && (outFile = getProjectFile( projectName, outFile)) == null)
          {
          throw new IllegalArgumentException( "Invalid outFile pattern='" + getOutFile() + "'");
          }

        String testDef = getTestDef();
        if( testDef != null && (testDef = getProjectFile( projectName, testDef)) == null)
          {
          throw new IllegalArgumentException( "Invalid testDef pattern='" + getTestDef() + "'");
          }

        String genDef = getGenDef();
        if( genDef != null && (genDef = getProjectFile( projectName, genDef)) == null)
          {
          throw new IllegalArgumentException( "Invalid genDef pattern='" + getGenDef() + "'");
          }


        // Set options for this Tcases project.
        Options options = new Options();
        options.setInputDef( inputDef);
        options.setGenDef( genDef==null? null : new File( inputDir, genDef));
        options.setOutDir( outDir);
        options.setTestDef( testDef==null? null : new File( outDir, testDef));
        options.setOutFile( outFile==null? null : new File( outDir, outFile));
        options.setJUnit( isJUnit());
        options.setExtended( !isNewTests());
        options.setRandomSeed( getSeed());
        options.setDefaultTupleSize( getTuples());

        File transformDef = getTransformDef();
        if( transformDef != null)
          {
          if( !transformDef.isAbsolute())
            {
            String projectTransformDef = getProjectFile( projectName, transformDef.getPath());
            if( projectTransformDef == null)
              {
              throw new IllegalArgumentException( "Invalid transformDef pattern='" + transformDef + "'");
              }
            transformDef = new File( inputDir, projectTransformDef);
            }
          options.setTransformDef( transformDef);
          }
        options.setTransformParams( getTransformParams());

        // Generate test cases for this Tcases project.
        getLog().info( "Generating tests for " + inputFile);
        tcases.run( options);
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't generate test cases", e);
      }
    }

  /**
   * Returns the file name defined by applying the given pattern to the project name.
   * Returns null if the file pattern is invalid.
   */
  private String getProjectFile( String projectName, String filePattern)
    {
    String projectFile = null;
    if( !StringUtils.isBlank( filePattern))
      {
      Matcher matcher = projectFilePattern_.matcher( filePattern);
      if( matcher.matches())
        {
        projectFile =
          StringUtils.isBlank( matcher.group(2))
          ? filePattern
          : matcher.group(1) + projectName + matcher.group(3);
        }
      }
    
    return projectFile;
    }

  /**
   * Changes the system input definition paths.
   */
  public void setInputDefs( Set<String> inputDefs)
    {
    this.inputDefs = inputDefs;
    }

  /**
   * Returns the system input definition paths.
   */
  public Set<String> getInputDefs()
    {
    return inputDefs;
    }

  /**
   * Changes the system input definition paths.
   */
  public void setInputDef( String inputDef)
    {
    this.inputDef = inputDef;
    }

  /**
   * Returns the system input definition paths.
   */
  public String getInputDef()
    {
    return inputDef;
    }

  /**
   * Changes the directory that contains system input definitions.
   */
  public void setInputDir( File inputDir)
    {
    this.inputDir = inputDir;
    }

  /**
   * Returns the directory that contains system input definitions.
   */
  public File getInputDir()
    {
    return inputDir;
    }

  /**
   * Changes the default tuple size for all generators.
   */
  public void setTuples( Integer tuples)
    {
    this.defaultTupleSize = tuples;
    }

  /**
   * Returns the default tuple size for all generators.
   */
  public Integer getTuples()
    {
    return defaultTupleSize;
    }

  /**
   * Changes the test definition output file pattern.
   */
  public void setOutFile( String outFile)
    {
    this.outFile = outFile;
    }

  /**
   * Returns the test definition output file pattern.
   */
  public String getOutFile()
    {
    return outFile;
    }

  /**
   * Changes the generator definition path.
   */
  public void setGenDef( String genDef)
    {
    this.genDef = genDef;
    }

  /**
   * Returns the generator definition path.
   */
  public String getGenDef()
    {
    return genDef;
    }

  /**
   * Changes if using the JUnit transform.
   */
  public void setJUnit( boolean junit)
    {
    this.junit = junit;
    }

  /**
   * Returns if using the JUnit transform.
   */
  public boolean isJUnit()
    {
    return junit;
    }

  /**
   * Changes the log output file for Tcases.
   */
  public void setLogFile( File logFile)
    {
    this.logFile = logFile;
    }

  /**
   * Returns the log output file for Tcases.
   */
  public File getLogFile()
    {
    return logFile;
    }

  /**
   * Changes if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public void setNewTests( boolean newTests)
    {
    this.newTests = newTests;
    }

  /**
   * Returns if previous contents of the test definition file are ignored.
   * If false, new test definitions are based on the previous test definitions.
   */
  public boolean isNewTests()
    {
    return newTests;
    }

  /**
   * Changes the output directory for generated test definitions.
   */
  public void setOutDir( File outDir)
    {
    this.outDir = outDir;
    }

  /**
   * Returns the output directory for generated test definitions.
   */
  public File getOutDir()
    {
    return outDir;
    }

  /**
   * Changes the transform parameter bindings.
   */
  public void setTransformParams( Map<String,Object> params)
    {
    this.transformParams = params;
    }

  /**
   * Returns the transform parameter bindings.
   */
  public Map<String,Object> getTransformParams()
    {
    return transformParams;
    }

  /**
   * Changes the random seed used by generators.
   */
  public void setSeed( Long seed)
    {
    this.seed = seed;
    }

  /**
   * Returns the random seed used by generators.
   */
  public Long getSeed()
    {
    return seed;
    }

  /**
   * Changes the transform file.
   */
  public void setTransformDef( File transformDef)
    {
    this.transformDef = transformDef;
    }

  /**
   * Returns the transform file.
   */
  public File getTransformDef()
    {
    return transformDef;
    }

  /**

   * Changes the test definition input file pattern.
   */
  public void setTestDef( String testDef)
    {
    this.testDef = testDef;
    }

  /**
   * Returns the test definition input file pattern.
   */
  public String getTestDef()
    {
    return testDef;
    }

  /**
   * Defines the path to the Tcases log file.
   */
  @Parameter(property="logFile",defaultValue="${project.build.directory}/logs/tcases.log")
  private File logFile;
  
  /**
   * Defines a set of patterns that match the system input definition files read by Tcases.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files read by Tcases.
   */
  @Parameter(property="inputDef",defaultValue="**/*-Input.xml")
  private String inputDef;

  /**
   * Defines the path to the directory where system input definition files are located.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases")
  private File inputDir;

  /**
   * Defines the path to the directory where Tcases output is written.
   */
  @Parameter(property="outDir",defaultValue="${project.build.directory}/tcases")
  private File outDir;

  /**
   * Defines the pattern for the output files generated by Tcases.
   */
  @Parameter(property="outFile")
  private String outFile;

  /**
   * Defines the pattern for the initial test case definition files read by Tcases.
   */
  @Parameter(property="testDef")
  private String testDef;

  /**
   * Defines the pattern for the generator definition files read by Tcases.
   */
  @Parameter(property="genDef")
  private String genDef;

  /**
   * Defines the path to the transform file applied by Tcases.
   */
  @Parameter(property="transformDef")
  private File transformDef;

  /**
   * Defines values for the transform parameters used by the transform file.
   */
  @Parameter(property="transformParams")
  private Map<String,Object> transformParams;

  /**
   * If true, generate test cases in the form of JUnit/TestNG test methods.
   */
  @Parameter(property="jUnit",defaultValue="false")
  private boolean junit;

  /**
   * If true, ignore any initial test definitions. Otherwise, generate new test definitions
   * that extend the initial test definitions.
   */
  @Parameter(property="newTests",defaultValue="false")
  private boolean newTests;

  /**
   * If defined, use the given random number for all generators. This updates the generator definitions specified
   * by the <CODE>genDef</CODE> parameter.
   */
  @Parameter(property="seed")
  private Long seed;

  /**
   * If defined, use the given default tuple size for all generators. This updates the generator definitions specified
   * by the <CODE>genDef</CODE> parameter.
   */
  @Parameter(property="defaultTupleSize")
  private Integer defaultTupleSize;

  private static final Pattern projectFilePattern_ = Pattern.compile( "([^\\*]*)(\\*?)([^\\*]*)");
  }
