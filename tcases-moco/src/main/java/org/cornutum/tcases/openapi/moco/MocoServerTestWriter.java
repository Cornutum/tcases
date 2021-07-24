//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.io.IndentedWriter;
import org.cornutum.tcases.openapi.resolver.RequestCase;
import org.cornutum.tcases.openapi.testwriter.JUnitTestWriter;
import org.cornutum.tcases.openapi.testwriter.JavaTestTarget;
import org.cornutum.tcases.openapi.testwriter.TestCaseWriter;
import org.cornutum.tcases.util.ListBuilder;
import org.cornutum.tcases.util.ToString;

import java.net.URI;
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
  protected MocoServerTestWriter( MocoServerConfig serverConfig, CertConfig certConfig, TestCaseWriter testCaseWriter)
    {
    super( testCaseWriter);
    setConfigWriter( writerFor( serverConfig));
    getConfigWriter().setCertConfigWriter( writerFor( certConfig));
    }

  /**
   * Returns a URI for the API server used by the given test case. If non-null, this supersedes
   * the server URI defined by this {@link RequestCase request case}.
   */
  @Override
  protected URI getTestServer( RequestCase requestCase)
    {
    return getConfigWriter().getTestServer( requestCase);
    }

  /**
   * Returns the URI scheme used in requests to the Moco server.
   */
  public String getServerScheme()
    {
    // By default, use "http"
    return "http";
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
  @Override
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
  @Override
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
   * Changes the {@link MocoServerTestWriter.ConfigWriter} for this test writer.
   */
  private void setConfigWriter( ConfigWriter<?> configWriter)
    {
    configWriter_ = configWriter;
    }

  /**
   * Returns the {@link MocoServerTestWriter.ConfigWriter} for this test writer.
   */
  protected ConfigWriter<?> getConfigWriter()
    {
    return configWriter_;
    }
  
  private ConfigWriter<?> writerFor( MocoServerConfig config)
    {
    ConfigWriterVisitor visitor = new ConfigWriterVisitor();
    config.accept( visitor);
    return visitor.writerFor_;
    }
  
  private Optional<CertConfigWriter<?>> writerFor( CertConfig certConfig)
    {
    return
      Optional.ofNullable( certConfig)
      .map( config -> {
        CertConfigWriterVisitor visitor = new CertConfigWriterVisitor();
        config.accept( visitor);
        return visitor.certConfigWriter_;
        });
    }

  @Override
  public String toString()
    {
    return
      ToString.getBuilder( this)
      .append( getConfigWriter().getConfig())
      .build();
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
    public void writeConfigDependencies( IndentedWriter targetWriter)
      {
      targetWriter.println( String.format( "import org.junit.%s;", getConfig().getRuleType()));
      getCertConfigWriter().ifPresent( cert -> cert.writeConfigDependencies( targetWriter));
      }

    /**
     * Writes server configuration initializations to the given stream.
     */
    public void writeConfigInit( IndentedWriter targetWriter)
      {
      getCertConfigWriter().ifPresent( cert -> cert.writeConfigInit( targetWriter));
      }

    /**
     * Writes the server configuration TestRule to the given stream.
     */
    public void writeConfigRule( IndentedWriter targetWriter)
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
    public List<String> getServerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();

      args.add( String.valueOf( getConfig().getPort()));
      getCertConfigWriter().ifPresent( cert -> cert.getServerFactoryConfigArgs().forEach( certArg -> args.add( certArg)));

      return args.build();
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
    public abstract List<String> getRunnerFactoryConfigArgs();

    /**
     * Returns a URI for the API server used by the given test case. If non-null, this supersedes
     * the server URI defined by this {@link RequestCase request case}.
     */
    public URI getTestServer( RequestCase requestCase)
      {
      try
        {
        return new URI( getServerScheme(), null, "localhost", getConfig().getPort(), null, null, null);
        }
      catch( Exception e)
        {
        throw new IllegalArgumentException( "Can't define test server URI", e);
        }
      }

    /**
     * Changes the {@link HttpsServerTestWriter.CertConfigWriter} for this test writer.
     */
    private void setCertConfigWriter( Optional<CertConfigWriter<?>> certConfigWriter)
      {
      certConfigWriter_ = certConfigWriter;
      }

  /**
   * Returns the {@link HttpsServerTestWriter.CertConfigWriter} for this test writer.
   */
  protected Optional<CertConfigWriter<?>> getCertConfigWriter()
    {
    return certConfigWriter_;
    }

    private final C config_;
    private Optional<CertConfigWriter<?>> certConfigWriter_;
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
    @Override
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
    @Override
    public List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();

      args
        .add( String.valueOf( getConfig().getPort()))
        .add( String.format( "file( \"%s\")", getConfig().getPath()));

      getCertConfigWriter().ifPresent( cert -> cert.getRunnerFactoryConfigArgs().forEach( certArg -> args.add( certArg)));

      return args.build();
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
    @Override
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
    @Override
    public List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();

      args
        .add( String.valueOf( getConfig().getPort()))
        .add( String.format( "pathResource( \"%s\")", getConfig().getPath()));

      getCertConfigWriter().ifPresent( cert -> cert.getRunnerFactoryConfigArgs().forEach( certArg -> args.add( certArg)));

      return args.build();
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
    @Override
    public List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return args.add( getConfig().getName()).build();
      }

    /**
     * Writes server configuration dependencies to the given stream.
     */
    @Override
    public void writeConfigDependencies( IndentedWriter targetWriter)
      {
      super.writeConfigDependencies( targetWriter);
      writePojoDependencies( targetWriter);
      }

    /**
     * Writes server configuration initializations to the given stream.
     */
    @Override
    public void writeConfigInit( IndentedWriter targetWriter)
      {
      super.writeConfigInit( targetWriter);
      
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
    @Override
    public void visit( MocoServerConfigFile config)
      {
      writerFor_ = new ConfigFileWriter( config);
      }

    @Override
    public void visit( MocoServerConfigResource config)
      {
      writerFor_ = new ConfigResourceWriter( config);
      }
  
    @Override
    public void visit( MocoServerConfigPojo config)
      {
      writerFor_ = new ConfigPojoWriter( config);
      }

    private ConfigWriter<?> writerFor_;
    }

  /**
   * Base class for certificate configuration writers.
   */
  private static abstract class CertConfigWriter<C extends CertConfig>
    {
    /**
     * Creates a new CertConfigWriter instance.
     */
    protected CertConfigWriter( C config)
      {
      config_ = config;
      }

    /**
     * Returns the {@link CertConfig} for this test writer.
     */
    protected C getConfig()
      {
      return config_;
      }

    /**
     * Writes certificate configuration dependencies to the given stream.
     */
    public void writeConfigDependencies( IndentedWriter targetWriter)
      {
      targetWriter.println( "import com.github.dreamhead.moco.HttpsCertificate;");
      targetWriter.println( "import static com.github.dreamhead.moco.HttpsCertificate.certificate;");
      }

    /**
     * Writes certificate configuration initializations to the given stream.
     */
    public void writeConfigInit( IndentedWriter targetWriter)
      {
      targetWriter.println(
        String.format(
          "private static final HttpsCertificate %s = certificate( %s);",
          getConfig().getName(),
          getCertFactoryConfigArgs().stream().collect( joining( ", "))));
      }

    /**
     * Returns the certificate factory arguments for this certificate configuration.
     */
    protected abstract List<String> getCertFactoryConfigArgs();

    /**
     * Returns the server factory arguments for certificate configuration.
     */
    public List<String> getServerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return args.add( getConfig().getName()).build();
      }

    /**
     * Returns the MocoJunitRunner factory arguments for certificate configuration.
     */
    public List<String> getRunnerFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();
      return args.add( getConfig().getName()).build();
      }

    private final C config_;
    }

  /**
   * Writes test definitions based on {@link CertConfigFile} configuration.
   */
  private static class CertConfigFileWriter extends CertConfigWriter<CertConfigFile>
    {
    /**
     * Creates a new CertConfigFileWriter instance.
     */
    public CertConfigFileWriter( CertConfigFile certConfig)
      {
      super( certConfig);
      }

    /**
     * Returns the certificate factory arguments for this certificate configuration.
     */
    @Override
    protected List<String> getCertFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();

      args
        .add( String.format( "file( \"%s\")", getConfig().getPath()))
        .add( String.format( "\"%s\"", getConfig().getKeyStorePassword()))
        .add( String.format( "\"%s\"", getConfig().getCertPassword()));

      return args.build();
      }
    }

  /**
   * Writes test definitions based on {@link CertConfigResource} configuration.
   */
  private static class CertConfigResourceWriter extends CertConfigWriter<CertConfigResource>
    {
    /**
     * Creates a new CertConfigResourceWriter instance.
     */
    public CertConfigResourceWriter( CertConfigResource certConfig)
      {
      super( certConfig);
      }

    /**
     * Returns the certificate factory arguments for this certificate configuration.
     */
    @Override
    protected List<String> getCertFactoryConfigArgs()
      {
      ListBuilder<String> args = ListBuilder.to();

      args
        .add( String.format( "pathResource( \"%s\")", getConfig().getPath()))
        .add( String.format( "\"%s\"", getConfig().getKeyStorePassword()))
        .add( String.format( "\"%s\"", getConfig().getCertPassword()));

      return args.build();
      }
    }

  /**
   * Creates a {@link CertConfigWriter} for a {@link CertConfig} instance.
   */
  private static class CertConfigWriterVisitor implements CertConfigVisitor
    {    
    @Override
    public void visit( CertConfigFile certConfig)
      {
      certConfigWriter_ = new CertConfigFileWriter( certConfig);
      }

    @Override
    public void visit( CertConfigResource certConfig)
      {
      certConfigWriter_ = new CertConfigResourceWriter( certConfig);
      }

    private CertConfigWriter<?> certConfigWriter_;
    }

  }
