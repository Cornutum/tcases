//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.moco;

import org.cornutum.tcases.openapi.moco.MocoTestConfig.ServerType;

import javax.json.Json;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;

import java.util.Optional;
import java.util.stream.Stream;

/**
 * Converts between a {@link MocoTestConfig} and its corresponding {@link JsonObject}.
 */
public class MocoTestConfigJson
  {
  /**
   * Creates a new MocoTestConfigJson instance.
   */
  private MocoTestConfigJson()
    {
    // Static methods only
    }

  /**
   * Returns the JSON object that represents the given {@link MocoTestConfig}.
   */
  public static JsonObject toJson( MocoTestConfig testConfig)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( SERVER, String.valueOf( testConfig.getServerType()));
    builder.add( RUNNER, toJson( testConfig.getServerConfig()));
    Optional.ofNullable( testConfig.getCertConfig()).ifPresent( cert -> builder.add( CERTIFICATE, toJson( cert)));
    
    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given {@link MocoServerConfig}.
   */
  public static JsonObject toJson( MocoServerConfig serverConfig)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( CONFIG, ConfigSource.toJson( serverConfig));
    Optional.ofNullable( serverConfig.getName()).ifPresent( name -> builder.add( NAME, name));
    Optional.ofNullable( serverConfig.getPort()).ifPresent( port -> builder.add( PORT, port));
    if( serverConfig.isEachTest())
      {
      builder.add( EACH_TEST, true);
      }

    return builder.build();
    }

  /**
   * Returns the JSON object that represents the given {@link CertConfig}.
   */
  public static JsonObject toJson( CertConfig certConfig)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    builder.add( CONFIG, CertSource.toJson( certConfig));
    Optional.ofNullable( certConfig.getName()).ifPresent( name -> builder.add( NAME, name));
    builder.add( KEY_STORE_PASSWORD, certConfig.getKeyStorePassword());
    builder.add( CERT_PASSWORD, certConfig.getCertPassword());

    return builder.build();
    }

  /**
   * Returns the MocoTestConfig instance represented by the given JSON object.
   */
  public static MocoTestConfig asMocoTestConfig( JsonObject json)
    {
    MocoTestConfig testConfig = new MocoTestConfig();

    try
      {
      testConfig.setServerType(
        Optional.ofNullable( json.getString( SERVER, null))
        .map( ServerType::valueOf)
        .orElseThrow( () -> new MocoTestConfigException( "Server type undefined")));

      testConfig.setServerConfig(
        Optional.ofNullable( json.getJsonObject( RUNNER))
        .map( MocoTestConfigJson::asMocoServerConfig)
        .orElseThrow( () -> new MocoTestConfigException( "Server runner undefined")));

      testConfig.setCertConfig(
        Optional.ofNullable( json.getJsonObject( CERTIFICATE))
        .map( MocoTestConfigJson::asCertConfig)
        .orElse( null));

      if( testConfig.getServerType().equals( ServerType.HttpsServer) && testConfig.getCertConfig() == null)
        {
        throw new MocoTestConfigException( "No certificate defined for HttpsServer");
        }
      }
    catch( Exception e)
      {
      throw new MocoTestConfigException( "Can't get MocoTestConfig object", e);
      }
    
    return testConfig;
    }

  /**
   * Returns the MocoServerConfig instance represented by the given JSON object.
   */
  private static MocoServerConfig asMocoServerConfig( JsonObject json)
    {
    MocoServerConfig serverConfig;

    try
      {
      JsonObject source =
        Optional.ofNullable( json.getJsonObject( CONFIG))
        .orElseThrow( () -> new MocoTestConfigException( "Moco server configuration source undefined"));

      serverConfig =
        Stream.of(
          Optional.ofNullable( source.getString( FILE, null))
          .map( file -> (MocoServerConfig) new MocoServerConfigFile( file)),
        
          Optional.ofNullable( source.getString( RESOURCE, null))
          .map( resource -> (MocoServerConfig) new MocoServerConfigResource( resource)),

          Optional.ofNullable( source.getString( POJO_WRITER_FACTORY, null))
          .map( factory -> (MocoServerConfig) getMocoServerConfigPojo( factory)))

        .filter( Optional::isPresent)
        .map( Optional::get)
        .findFirst()
        .orElseThrow( () -> new MocoTestConfigException( "Moco server configuration source undefined"));

      serverConfig.setName( json.getString( NAME, null));
      serverConfig.setPort( Optional.ofNullable( json.getJsonNumber( PORT)).map( JsonNumber::intValueExact).orElse( null));
      serverConfig.setEachTest( json.getBoolean( EACH_TEST, false)); 
      }
    catch( Exception e)
      {
      throw new MocoTestConfigException( "Can't get MocoServerConfig object", e);
      }
    
    return serverConfig;
    }

  /**
   * Returns the {@link MocoServerConfigPojo} created by the given factory class.
   */
  private static MocoServerConfigPojo getMocoServerConfigPojo( String factoryName)
    {
    try
      {
      return new MocoServerConfigPojo( factoryName);
      }
    catch( Exception e)
      {
      throw new MocoTestConfigException( String.format( "Can't create MocoServerConfigPojo from factory=%s", factoryName), e);
      }
    }

  /**
   * Returns the CertConfig instance represented by the given JSON object.
   */
  private static CertConfig asCertConfig( JsonObject json)
    {
    CertConfig certConfig;

    try
      {
      String name = json.getString( NAME, null);

      String keyStorePassword =
        Optional.ofNullable( json.getString( KEY_STORE_PASSWORD, null))
        .orElseThrow( () -> new MocoTestConfigException( "Key store password undefined"));

      String certPassword =
        Optional.ofNullable( json.getString( CERT_PASSWORD, null))
        .orElseThrow( () -> new MocoTestConfigException( "Certificate password undefined"));
      
      JsonObject source =
        Optional.ofNullable( json.getJsonObject( CONFIG))
        .orElseThrow( () -> new MocoTestConfigException( "Moco certificate configuration source undefined"));

      certConfig =
        Stream.of(
          Optional.ofNullable( source.getString( FILE, null))
          .map( file -> (CertConfig) new CertConfigFile( name, file, keyStorePassword, certPassword)),

          Optional.ofNullable( source.getString( RESOURCE, null))
          .map( resource -> (CertConfig) new CertConfigResource( name, resource, keyStorePassword, certPassword)))

        .filter( Optional::isPresent)
        .map( Optional::get)
        .findFirst()
        .orElseThrow( () -> new MocoTestConfigException( "Moco certificate configuration source undefined"));
      }
    catch( Exception e)
      {
      throw new MocoTestConfigException( "Can't get CertConfig object", e);
      }
    
    return certConfig;
    }

  /**
   * Returns the JSON object that represents the source for Moco server configuration.
   */
  private static class ConfigSource implements ConfigVisitor
    {
    /**
     * Creates a new ConfigSource instance.
     */
    private ConfigSource()
      {
      }

    /**
     * Returns the JSON object that represents the source for the given Moco server configuration.
     */
    public static JsonObject toJson( MocoServerConfig serverConfig)
      {
      ConfigSource source = new ConfigSource();
      serverConfig.accept( source);
      return source.getJson();
      }

    /**
     * Returns the JSON object that represents the source for this Moco server configuration.
     */
    public JsonObject getJson()
      {
      return json_;
      }
    
    public void visit( MocoServerConfigFile config)
      {
      json_ =
        Json.createObjectBuilder()
        .add( FILE, config.getPath())
        .build();
      }

    public void visit( MocoServerConfigResource config)
      {
      json_ =
        Json.createObjectBuilder()
        .add( RESOURCE, config.getPath())
        .build();
      }
  
    public void visit( MocoServerConfigPojo config)
      {
      JsonObjectBuilder builder = Json.createObjectBuilder();
      if( config.getFactory() != null)
        {
        builder.add( POJO_WRITER_FACTORY, config.getFactory().getClass().getName());
        }
      else
        {
        builder.add( POJO_WRITER_FACTORY, JsonValue.NULL);
        }
      
      json_ = builder.build();
      }
  
    private JsonObject json_;
    }

  /**
   * Returns the JSON object that represents the source for Moco certificate configuration.
   */
  private static class CertSource implements CertConfigVisitor
    {
    /**
     * Creates a new CertSource instance.
     */
    private CertSource()
      {
      }

    /**
     * Returns the JSON object that represents the source for the given Moco certificate configuration.
     */
    public static JsonObject toJson( CertConfig certConfig)
      {
      CertSource source = new CertSource();
      certConfig.accept( source);
      return source.getJson();
      }

    /**
     * Returns the JSON object that represents the source for this Moco certificate configuration.
     */
    public JsonObject getJson()
      {
      return json_;
      }
    
    public void visit( CertConfigFile config)
      {
      json_ =
        Json.createObjectBuilder()
        .add( FILE, config.getPath())
        .build();
      }

    public void visit( CertConfigResource config)
      {
      json_ =
        Json.createObjectBuilder()
        .add( RESOURCE, config.getPath())
        .build();
      }
  
    private JsonObject json_;
    }

  private static final String CERTIFICATE = "certificate";
  private static final String CERT_PASSWORD = "certPassword";
  private static final String CONFIG = "config";
  private static final String EACH_TEST = "eachTest";
  private static final String FILE = "file";
  private static final String KEY_STORE_PASSWORD = "keyStorePassword";
  private static final String NAME = "name";
  private static final String POJO_WRITER_FACTORY = "pojoWriterFactory";
  private static final String PORT = "port";
  private static final String RESOURCE = "resource";
  private static final String RUNNER = "runner";
  private static final String SERVER = "server";
  }
