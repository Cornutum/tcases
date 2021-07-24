package org.cornutum.tcases.maven;

import org.cornutum.tcases.ReducerCommand.Options;
import org.cornutum.tcases.ReducerCommand;
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;

import java.io.File;
import java.util.Set;

/**
 * Runs the Tcases Reducer. For full details on the Reducer &mdash; what it does and how it works &mdash; see
 * <A href="http://www.cornutum.org/tcases/docs/Tcases-Guide.htm">Tcases: The Complete Guide</A> at www.cornutum.org.
 *
 */
@Mojo(name="reduce")
public class ReducerMojo extends AbstractMojo
  {
  @Override
  public void execute() throws MojoExecutionException
    {
    try
      {
      // Gather input definition files
      DirectoryScanner inputScanner = new DirectoryScanner();
      Set<String> inputDefPatterns = getInputDefs();
      if( !inputDefPatterns.isEmpty())
        {
        // Use all specified input definition patterns.
        }
      else if( !StringUtils.isBlank( getProject()))
        {
        // Use input definition(s) for specified project name.
        inputDefPatterns.add( "**/" + getProject() + "-Input.xml");
        inputDefPatterns.add( "**/" + getProject() + ".xml");
        inputDefPatterns.add( "**/" + getProject() + "-Input.json");
        inputDefPatterns.add( "**/" + getProject() + ".json");
        }
      else if( !StringUtils.isBlank( getInputDef())) 
        {
        // Use specified input definition pattern.
        inputDefPatterns.add( getInputDef());
        }
      else
        {
        // Use default patterns
        inputDefPatterns.add( "**/*-Input.xml");
        inputDefPatterns.add( "**/*-Input.json");
        }
      inputScanner.setIncludes( inputDefPatterns.toArray( new String[0]));

      File inputRootDir = getInputDirFile();
      inputScanner.setBasedir( inputRootDir);
      inputScanner.scan();

      // Reduce test cases for each input definition file.
      ReducerCommand reducer = new ReducerCommand();
      String[] inputDefs = inputScanner.getIncludedFiles();
      for( int i = 0; i < inputDefs.length; i++)
        {
        // Define input path for this Tcases project.
        String inputFile = inputDefs[i];
        String inputPath = FileUtils.dirname( inputFile);
        File inputDef = new File( inputRootDir, inputFile);
        File inputDir = new File( inputRootDir, inputPath);

        String projectName = getProjectName( inputDef);

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
        
        // Set Reducer options for this Tcases project.
        Options options = new Options();
        options.setInputDef( inputDef);
        options.setGenDef( genDef==null? null : new File( inputDir, genDef));
        options.setTestDef( testDef==null? null : new File( inputDir, testDef));
        options.setFunction( getFunction());
        options.setSamples( getSamples());
        options.setResampleFactor( getResampleFactor());
        options.setNewSeed( isNewSeed());
        options.setContentType( getContentType());

        // Reduce test cases for this Tcases project.
        reducer.run( options);
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't reduce test cases", e);
      }
    }

  /**
   * If the given path is not absolute, returns it as an absolute path relative to the
   * project base directory. Otherwise, returns the given absolute path.
   */
  private File getBaseDir( File path)
    {
    return getDirPath( baseDir_, path);
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
   * Changes the system input definition path.
   */
  public void setInputDef( String inputDef)
    {
    this.inputDef = inputDef;
    }

  /**
   * Returns the system input definition path.
   */
  public String getInputDef()
    {
    return inputDef;
    }

  /**
   * Changes the system input definition project name.
   */
  public void setProject( String project)
    {
    this.project = project;
    }

  /**
   * Returns the system input definition project name.
   */
  public String getProject()
    {
    return project;
    }

  /**
   * Changes the directory that contains system input definitions.
   */
  public void setInputDir( String inputDir)
    {
    this.inputDir = inputDir;
    }

  /**
   * Returns the directory that contains system input definitions.
   */
  public String getInputDir()
    {
    return inputDir;
    }

  /**
   * Returns the directory that contains system input definitions.
   */
  public File getInputDirFile()
    {
    return getBaseDir( inputDir==null? null : new File( inputDir));
    }

  /**
   * Changes the default content type.
   */
  public void setContentType( String contentType)
    {
    this.contentType = contentType;
    }

  /**
   * Returns the default content type.
   */
  public String getContentType()
    {
    return contentType;
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

   * Changes the base test definition input file pattern.
   */
  public void setTestDef( String testDef)
    {
    this.testDef = testDef;
    }

  /**
   * Returns the base test definition input file pattern.
   */
  public String getTestDef()
    {
    return testDef;
    }

  /**
   * Changes the function for which tests cases are reduced.
   */
  public void setFunction( String function)
    {
    this.function = function;
    }

  /**
   * Returns the function for which tests cases are reduced.
   */
  public String getFunction()
    {
    return function;
    }

  /**
   * Changes the resample factor.
   * The <CODE>resampleFactor</CODE> determines the number of samples in the next round of reducing.
   * Depending on the <CODE>resampleFactor</CODE>, the next round may use more or fewer samples.
   * <P/>
   * If the previous round called for <CODE>N</CODE> samples and produced a reduction, then the number of samples for the
   * next round will be <CODE>N * ( 1 + resampleFactor)</CODE>. To increase sample count with each round, define
   * <CODE>resampleFactor</CODE> &gt; 0.  To decrease sample count with each round, define -1 &lt;
   * <CODE>resampleFactor</CODE> &lt; 0.
   */
  public void setResampleFactor( double resampleFactor)
    {
    this.resampleFactor = resampleFactor;
    }

  /**
   * Returns the {@link #setResampleFactor resample factor}.
   */
  public double getResampleFactor()
    {
    return resampleFactor;
    }

  /**
   * Changes the initial number of samples.
   */
  public void setSamples( int samples)
    {
    this.samples = samples;
    }

  /**
   * Returns the initial number of samples.
   */
  public int getSamples()
    {
    return samples;
    }

  /**
   * Changes if ignoring current random seed used by generators.
   */
  public void setNewSeed( boolean newSeed)
    {
    this.newSeed = newSeed;
    }

  /**
   * Returns if ignoring current random seed used by generators.
   */
  public boolean isNewSeed()
    {
    return newSeed;
    }
  
  /**
   * Defines a set of patterns that match the system input definition files read by Reducer.
   * By default, Reducer uses the single pattern defined by the <B><CODE>inputDef</CODE></B> parameter.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files read by Reducer,
   * relative to the directory specified by the <B><CODE>inputDir</CODE></B>.
   * If omitted, the default value matches all files of the form "*-Input.xml" or "*-Input.json".
   * <P/>
   * An input definition file defines a <EM>project name</EM> that is used to form the
   * default names for other associated files. For an input definition file of the form
   * "${prefix}-Input.xml", the project name is "${prefix}". Otherwise, the project name
   * is the basename of the input definition file.
   */
  @Parameter(property="inputDef")
  private String inputDef;
  
  /**
   * A short-hand form of the <B><CODE>inputDefs</CODE></B> parameter that makes it easier
   * to select the system input definition for a specific project. Equivalent to setting
   * <B><CODE>inputDefs</CODE></B> to
   * <CODE>&lowast;&lowast;/${project}-Input.xml,&lowast;&lowast;/${project}.xml,&lowast;&lowast;/${project}-Input.json,&lowast;&lowast;/${project}.json</CODE>.
   */
  @Parameter(property="project")
  private String project;

  /**
   * Defines the path to the directory where system input definition files are located.
   * A relative path is applied relative to the <B><CODE>${basedir}</CODE></B> of
   * the project.
   */
  @Parameter(property="inputDir",defaultValue="${basedir}/src/test/tcases")
  private String inputDir;

  /**
   * Defines the default content type for files that are read and produced. The <B><CODE>contentType</CODE></B> must be one of "json" or "xml".
   * The default content type is assumed for any file that is not specified explicitly or that does not have a recognized extension.
   * If omitted, the default content type is derived from the input definition file name.
   */
  @Parameter(property="contentType")
  private String contentType;

  /**
   * Defines the name for the initial test case definition files read by Reducer for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Test.xml" or "*-Test.json" (depending on the <B><CODE>contentType</CODE></B>)
   * or, if the <B><CODE>junit</CODE></B> parameter is true, "*Test.java".
   */
  @Parameter(property="testDef")
  private String testDef;

  /**
   * Defines the name for the updated generator definition file written by Reducer for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Generators.xml" or "*-Generators.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="genDef")
  private String genDef;

  /**
   * Defines the function for which tests cases are reduced.
   * If undefined, reduce test cases for all functions.
   */
  @Parameter(property="function")
  private String function;

  /**
   * Defines the resample factor.
   * The <CODE>resampleFactor</CODE> determines the number of samples in the next round of reducing.
   * Depending on the <CODE>resampleFactor</CODE>, the next round may use more or fewer samples.
   * <P/>
   * If the previous round called for <CODE>N</CODE> samples and produced a reduction, then the number of samples for the
   * next round will be <CODE>N * ( 1 + resampleFactor)</CODE>. To increase sample count with each round, define
   * <CODE>resampleFactor</CODE> &gt; 0.  To decrease sample count with each round, define -1 &lt;
   * <CODE>resampleFactor</CODE> &lt; 0.
   */
  @Parameter(property="resampleFactor",defaultValue="0.0")
  private double resampleFactor;

  /**
   * If true, ignore any random seed defined by generators and search for a new seed to reduce test cases.
   */
  @Parameter(property="newSeed",defaultValue="false")
  private boolean newSeed;

  /**
   * Defines the number of samples for the initial round of reducing.
   */
  @Parameter(property="samples",defaultValue="10")
  private int samples;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;
  }
