//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.testwriter.JUnitTestWriter;
import org.cornutum.tcases.openapi.testwriter.JavaTestTarget;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;
import org.cornutum.tcases.util.ListBuilder;

import java.util.List;
import java.util.Optional;
import static java.util.stream.Collectors.joining;

/**
 * A {@link JUnitTestWriter} for API tests that use a <a href="https://github.com/dreamhead/moco/blob/master/moco-doc/junit.md">Moco server</a>
 */
public abstract class MocoServerTestWriter extends JUnitTestWriter
  {
  /**
   * Creates a new MocoServerTestWriter instance.
   */
  protected MocoServerTestWriter( MocoServerConfig serverConfig, TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    setConfigWriter( writerFor( serverConfig));
    }

  /**
   * Returns the Moco server class name for this test writer.
   */
  protected abstract String getServerClass();

  /**
   * Returns the Moco server factory method for this test writer.
   */
  protected abstract String getServerFactory();

  /**
   * Returns the Moco server factory arguments for this test writer.
   */
  protected List<String> getServerFactoryArgs()
    {
    return getConfigWriter().getServerFactoryConfigArgs();
    }

  /**
   * Returns the MocoJunitRunner factory method for this test writer.
   */
  protected abstract String getRunnerFactory();

  /**
   * Returns the MocoJunitRunner factory arguments for this test writer.
   */
  protected List<String> getRunnerFactoryArgs()
    {
    return getConfigWriter().getRunnerFactoryConfigArgs();
    }

  /**
   * Writes the target test dependencies to the given stream.
   */
  protected void writeDependencies( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    super.writeDependencies( target, testName, targetWriter);

    targetWriter.println();
    targetWriter.println( "import com.github.dreamhead.moco.junit.MocoJunitRunner;");
    targetWriter.println( "import static com.github.dreamhead.moco.Moco.*;");

    writeConfigDependencies( targetWriter);
    }

  /**
   * Writes the target test dependencies for the POJO server configuration to the given stream.
   */
  protected abstract void writePojoDependencies( IndentedWriter targetWriter);

  /**
   * Writes the target test declarations to the given stream.
   */
  protected void writeDeclarations( JavaTestTarget target, String testName, IndentedWriter targetWriter)
    {
    super.writeDeclarations( target, testName, targetWriter);

    writeConfigInit( targetWriter);
    writeConfigRule( targetWriter);
    }

  /**
   * Writes server configuration dependencies to the given stream.
   */
  protected void writeConfigDependencies( IndentedWriter targetWriter)
    {
    getConfigWriter().writeConfigDependencies( targetWriter);
    }

  /**
   * Writes server configuration initializations to the given stream.
   */
  protected void writeConfigInit( IndentedWriter targetWriter)
    {
    getConfigWriter().writeConfigInit( targetWriter);
    }

  /**
   * Writes the server configuration TestRule to the given stream.
   */
  protected void writeConfigRule( IndentedWriter targetWriter)
    {
    getConfigWriter().writeConfigRule( targetWriter);
    } 

  /**
   * Changes the {@link ConfigWriter} for this test writer.
   */
  private void setConfigWriter( ConfigWriter<?> configWriter)
    {
    configWriter_ = configWriter;
    }

  /**
   * Returns the {@link ConfigWriter} for this test writer.
   */
  protected ConfigWriter<?> getConfigWriter()
    {
    return configWriter_;
    }
  
  private ConfigWriter<?> writerFor( MocoServerConfig config)
    {
    ConfigWriterVisitor visitor = new ConfigWriterVisitor();
    config.accept( visitor);
    return visitor.configWriter_;
    }


  private ConfigWriter<?> configWriter_;
  
  /**
   * Base class for server configuration writers.
   */
  private abstract class ConfigWriter<C extends MocoServerConfig>
    {
    /**
     * Creates a new ConfigWriter instance.
     */
    protected ConfigWriter( C config)
      {
      config_ = config;
      }

    /**
     * Returns the {@link MocoServerConfig} for this test writer.
     */
    protected C getConfig()
      {
      return config_;
      }

    /**
     * Writes server configuration dependencies to the given stream.
     */
    protected void writeConfigDependencies( IndentedWriter targetWriter)
      {
      targetWriter.println( String.format( "import org.junit.%s;", getConfig().getRuleType()));
      }

    /**
     * Writes server configuration initializations to the given stream.
     */
    protected void writeConfigInit( IndentedWriter targetWriter)
      {
      // By default, none.
      }

    /**
     * Writes the server configuration TestRule to the given stream.
     */
    protected void writeConfigRule( IndentedWriter targetWriter)
      {
      String ruleType = getConfig().getRuleType();

      targetWriter.println();
      targetWriter.println( String.format( "@%s", ruleType));

      targetWriter.println(
        String.format(
          "public %sMocoJunitRunner runner = MocoJunitRunner.%s( %s);",
          ruleType.equals( "ClassRule")? "static " : "",
          getRunnerFactoryConfig(),
          getRunnerFactoryArgs().stream().collect( joining( ", "))));
      }

    /**
     * Returns the Moco server factory arguments for this server configuration.
     */
    protected List<String> getServerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return args.add( String.valueOf( getConfig().getPort())).build();
      }

    /**
     * Returns the MocoJunitRunner factory method for this server configuration.
     */
    protected String getRunnerFactoryConfig()
      {
      return getRunnerFactory();
      }

    /**
     * Returns the MocoJunitRunner factory arguments for server configuration.
     */
    protected abstract List<String> getRunnerFactoryConfigArgs();

    private final C config_;
    }

  /**
   * Writes test definitions based on {@link MocoServerConfigFile} configuration.
   */
  private class ConfigFileWriter extends ConfigWriter<MocoServerConfigFile>
    {
    /**
     * Creates a new ConfigFileWriter instance.
     */
    public ConfigFileWriter( MocoServerConfigFile config)
      {
      super( config);
      }

    /**
     * Returns the MocoJunitRunner factory method for this server configuration.
     */
    protected String getRunnerFactoryConfig()
      {
      return
        String.format(
          "json%s%s",
          getRunnerFactory().substring( 0, 1).toUpperCase(),
          getRunnerFactory().substring( 1));
      }

    /**
     * Returns the MocoJunitRunner factory arguments for server configuration.
     */
    protected List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return
        args
        .add( String.valueOf( getConfig().getPort()))
        .add( String.format( "\"%s\"", getConfig().getPath()))
        .build();
      }
    }

  /**
   * Writes test definitions based on {@link MocoServerConfigResource} configuration.
   */
  private class ConfigResourceWriter extends ConfigWriter<MocoServerConfigResource>
    {
    /**
     * Creates a new ConfigResourceWriter instance.
     */
    public ConfigResourceWriter( MocoServerConfigResource config)
      {
      super( config);
      }

    /**
     * Returns the MocoJunitRunner factory method for this server configuration.
     */
    protected String getRunnerFactoryConfig()
      {
      return
        String.format(
          "json%s%s",
          getRunnerFactory().substring( 0, 1).toUpperCase(),
          getRunnerFactory().substring( 1));
      }

    /**
     * Returns the MocoJunitRunner factory arguments for server configuration.
     */
    protected List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return
        args
        .add( String.valueOf( getConfig().getPort()))
        .add( String.format( "pathResource( \"%s\")", getConfig().getPath()))
        .build();
      }
    }

  /**
   * Writes test definitions based on {@link MocoServerConfigPojo} configuration.
   */
  private class ConfigPojoWriter extends ConfigWriter<MocoServerConfigPojo>
    {
    /**
     * Creates a new ConfigPojoWriter instance.
     */
    public ConfigPojoWriter( MocoServerConfigPojo config)
      {
      super( config);
      }

    /**
     * Returns the MocoJunitRunner factory arguments for server configuration.
     */
    protected List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return
        args
        .add( getConfig().getName())
        .build();
      }

    /**
     * Writes server configuration dependencies to the given stream.
     */
    protected void writeConfigDependencies( IndentedWriter targetWriter)
      {
      super.writeConfigDependencies( targetWriter);
      writePojoDependencies( targetWriter);
      }

    /**
     * Writes server configuration initializations to the given stream.
     */
    protected void writeConfigInit( IndentedWriter targetWriter)
      {
      targetWriter.println();
      targetWriter.println( String.format( "private static %s %s;", getServerClass(), getConfig().getName()));

      targetWriter.println();
      targetWriter.println( "static {");
      targetWriter.indent();

      targetWriter.println(
        String.format(
          "%s = %s( %s);",
          getConfig().getName(),
          getServerFactory(),
          getServerFactoryArgs().stream().collect( joining( ", "))));

      Optional.ofNullable( getConfig().getPojoWriter())
        .ifPresent( pojoWriter -> pojoWriter.writePojo( getConfig().getName(), targetWriter));

      targetWriter.unindent();
      targetWriter.println( "}");
      }
    }

  /**
   * Creates a {@link ConfigWriter} for a {@link MocoServerConfig} instance.
   */
  private class ConfigWriterVisitor implements ConfigVisitor
    {    
    public void visit( MocoServerConfigFile config)
      {
      configWriter_ = new ConfigFileWriter( config);
      }

    public void visit( MocoServerConfigResource config)
      {
      configWriter_ = new ConfigResourceWriter( config);
      }
  
    public void visit( MocoServerConfigPojo config)
      {
      configWriter_ = new ConfigPojoWriter( config);
      }

    private ConfigWriter<?> configWriter_;
    }

  }
