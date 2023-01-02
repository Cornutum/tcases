package org.cornutum.tcases.maven;

import org.cornutum.tcases.CopyCommand.Options;
import static org.cornutum.tcases.CommandUtils.*;
import static org.cornutum.tcases.maven.MojoUtils.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.codehaus.plexus.util.DirectoryScanner;
import org.codehaus.plexus.util.FileUtils;
import org.cornutum.tcases.CopyCommand;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Set;

/**
 * Copies a Tcases project.
 */
@Mojo(name="copy")
public class CopyMojo extends AbstractMojo
  {
  @Override
  public void execute() throws MojoExecutionException
    {
    try
      {
      File inputRootDir = getInputDirFile();
      if( !inputRootDir.exists())
        {
        logger_.warn( "Input directory={} does not exist", inputRootDir);
        }

      else
        {
        // Gather input definition files
        DirectoryScanner inputScanner = new DirectoryScanner();
        inputScanner.setBasedir( inputRootDir);

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
        inputScanner.scan();

        // Copy project files for each input definition file.
        File destRootDir = getDestDirFile();
        String[] inputDefs = inputScanner.getIncludedFiles();
        for( int i = 0; i < inputDefs.length; i++)
          {
          // Define input and output paths for this Tcases project.
          String inputFile = inputDefs[i];
          String inputPath = FileUtils.dirname( inputFile);
          File inputDef = new File( inputRootDir, inputFile);
          File inputDir = new File( inputRootDir, inputPath);
          File destDir = new File( destRootDir, inputPath);

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


          // Set options for this Tcases project.
          Options options = new Options();
          options.setInputDef( inputDef);
          options.setGenDef( genDef==null? null : new File( inputDir, genDef));
          options.setTestDef( testDef==null? null : new File( inputDir, testDef));
          options.setContentType( getContentType());
          options.setDestDir( destDir);
          options.setDestType( getDestType());
          options.setDestName( getDestName());

          // Copy files for this Tcases project.
          CopyCommand.run( options);
          }
        }
      }
    catch( Exception e)
      {
      throw new MojoExecutionException( "Can't copy project files", e);
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
   * Changes the output directory for copied project files.
   */
  public void setDestDir( String destDir)
    {
    this.destDir = destDir;
    }

  /**
   * Returns the output directory for copied project files.
   */
  public String getDestDir()
    {
    return destDir;
    }

  /**
   * Returns the output directory for copied project files.
   */
  public File getDestDirFile()
    {
    return getDirPath( getInputDirFile(), destDir==null? null : new File( destDir));
    }

  /**
   * Changes the project name used for copied files.
   */
  public void setDestName( String destName)
    {
    this.destName = destName;
    }

  /**
   * Returns the project name used for copied files.
   */
  public String getDestName()
    {
    return destName;
    }

  /**
   * Changes the content type used for copied files.
   */
  public void setDestType( String destType)
    {
    this.destType = destType;
    }

  /**
   * Returns the content type used for copied files.
   */
  public String getDestType()
    {
    return destType;
    }

  /**
   * Defines a set of patterns that match the system input definition files read by Tcases.
   * By default, Tcases uses the single pattern defined by the <B><CODE>inputDef</CODE></B> parameter.
   */
  @Parameter(property="inputDefs")
  private Set<String> inputDefs;

  /**
   * Defines a single pattern that matches the system input definition files read by Tcases,
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
   * Defines the path to the directory where copied project files are written.
   * A relative path is applied relative to the <B><CODE>inputDir</CODE></B> of
   * the project. If undefined, the default destination directory is the same
   * as the <B><CODE>inputDir</CODE></B>.
   */
  @Parameter(property="destDir")
  private String destDir;

  /**
   * If defined, copied files use the given project name. Otherwise, the original project name is used.
   */
  @Parameter(property="destName")
  private String destName;

  /**
   * If defined, copied files use the given content type. Otherwise, the original content type is used.
   * The <B><CODE>destType</CODE></B> must be one of "json" or "xml".
   */
  @Parameter(property="destType")
  private String destType;

  /**
   * Defines the default content type for project files. The <B><CODE>contentType</CODE></B> must be one of "json" or "xml".
   * If omitted, the default content type is derived from the input definition file name.
   */
  @Parameter(property="contentType")
  private String contentType;

  /**
   * Defines the name for the initial test case definition files read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Test.xml" or "*-Test.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="testDef")
  private String testDef;

  /**
   * Defines the name for the generator definition file read by Tcases for an input definition file.
   * This file name may contain at most one "*" wildcard character, in which case
   * the "*" is replaced by the <EM>project name</EM> of the corresponding
   * input definition file &mdash; see the <B><CODE>inputDef</CODE></B> parameter for
   * details.
   * The default value is "*-Generators.xml" or "*-Generators.json" (depending on the <B><CODE>contentType</CODE></B>).
   */
  @Parameter(property="genDef")
  private String genDef;

  @Parameter(readonly=true,defaultValue="${basedir}")
  private File baseDir_;

  @Parameter(readonly=true,defaultValue="${project.build.directory}")
  private File targetDir_;

  private static final Logger logger_ = LoggerFactory.getLogger( CopyMojo.class);
  }
