//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import static org.cornutum.tcases.openapi.test.CollectionUtils.mapping;
import static org.cornutum.tcases.openapi.test.CollectionUtils.toStream;
import static org.cornutum.tcases.openapi.test.JsonUtils.createObjectNode;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.IntStream;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Returns the JSON representation of application/x-www-form-urlencoded content.
 */
public class FormUrlDecoder extends AbstractDecoder
  {
  /**
   * Creates a new FormUrlDecoder instance.
   */
  public FormUrlDecoder( ContentDef contentDef)
    {
    super( contentDef.getValueEncoding());
    contentDef_ = contentDef;
    }

  /**
   * Returns the possible JSON array representations of the given content.
   */
  @Override
  public List<JsonNode> decodeArray( String content)
    {
    return emptyList();
    }

  /**
   * Returns the JSON number representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeNumber( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the JSON boolean representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeBoolean( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the JSON string representation of the given content.
   */
  @Override
  public Optional<JsonNode> decodeString( String content)
    {
    return Optional.empty();
    }

  /**
   * Returns the possible JSON object representations of the given content.
   */
  @Override
  public List<JsonNode> decodeObject( String content)
    {
    return
      // Is this a (possibly empty) list of property mappings?...
      Optional.ofNullable( content)
      .map( text -> text.isEmpty()? new String[0] : text.split( "&", -1))
      .map( pairs -> {

        List<String[]> mappings =
          Arrays.stream( pairs)
          .map( pair -> pair.split( "=", -1))
          .collect( toList());

        mappings.stream()
          .filter( mapping -> mapping.length != 2)
          .findFirst()
          .ifPresent( mapping -> {
            throw new IllegalStateException(
              String.format(
                "'%s' is not a valid key/value pair",
                Arrays.stream( mapping).collect( joining( "="))));
            });
        
        List<Map.Entry<Key,String>> bindings =
          mappings.stream()
          .map( mapping -> mapping( Key.of( getContentDef(), decodeUrl( mapping[0])), decodeUrl( mapping[1])))
          .collect( toList());

        // Return JSON representations for these form bindings.
        return decodeObject( bindings);
        })

      // No, not recognizable as an object
      .orElse( emptyList());
    }

  /**
   * Returns the possible JSON object representations of the given form bindings.
   */
  private List<JsonNode> decodeObject( List<Map.Entry<Key,String>> bindings)
    {
    List<JsonNode> decoded;

    if( bindings.isEmpty())
      {
      decoded = singletonList( createObjectNode());
      }
    else
      {
      Key key = bindings.get(0).getKey();

      List<Map.Entry<Key,String>> bindingsForProperty =
        bindings.subList(
          0,
          IntStream.range( 1, bindings.size())
          .filter( i -> !bindings.get(i).getKey().equalsFormProperty( key))
          .findFirst()
          .orElse( bindings.size()));

      String formProperty = key.getFormProperty();

      decoded =
        decodeProperty( formProperty, bindingsForProperty).stream()
        .map( propertyValue -> newObject( formProperty, propertyValue))
        .flatMap( firstProperty -> {
            return
              decodeObject( bindings.subList( bindingsForProperty.size(), bindings.size())).stream()
              .map( JsonUtils::expectObject)
              .map( otherProperties -> appendObject( firstProperty, otherProperties));
          })
        .collect( toList());
      }

    return decoded;
    }

  /**
   * Returns the possible JSON object representations for values of the given form property.
   */
  private List<JsonNode> decodeProperty( String formProperty, List<Map.Entry<Key,String>> bindings)
    {
    EncodingDef encoding = getPropertyEncoding( formProperty);

    return
      !encoding.isExploded()?
      Optional.of( bindings)
      .filter( singleton -> singleton.size() == 1)
      .map( singleton -> singleton.get(0))
      .map( binding -> {
        return
          forFormProperty( binding)
          .map( property -> decodeValue( encoding.getStyle(), property.getValue()))
          .orElseThrow( () -> new IllegalStateException( String.format( "Unexpected value for %s", binding.getKey())));
        })
      .orElseThrow( () -> new IllegalStateException( String.format( "Expected explode=false for property='%s' but found %s bindings", formProperty, bindings.size()))) :
      
      "deepObject".equals( encoding.getStyle())?
      decodeExplodedObject(
        Optional.of( bindings)
        .filter( deepBindings -> isExplodedObject( "deepObject", deepBindings))
        .orElseThrow( () -> new IllegalStateException( String.format( "Expected style=deepObject for property='%s' not found", formProperty)))) :

      decodeExploded( encoding.getStyle(), bindings);
    }

  /**
   * Returns the possible JSON object representations of the given exploded object value.
   */
  private List<JsonNode> decodeExplodedObject( List<Map.Entry<Key,String>> bindings)
    {
    List<JsonNode> decoded;
    
    if( bindings.isEmpty())
      {
      decoded = singletonList( createObjectNode());
      }
    else
      {
      Key key = bindings.get(0).getKey();
      String valueProperty = key.getValueProperty();
      String valuePropertyContent = bindings.get(0).getValue();

      decoded = 
        decodeValue( "simple", valuePropertyContent).stream()
        .map( json -> newObject( valueProperty, json))
        .flatMap( firstProperty -> {
          return
            decodeExplodedObject( bindings.subList( 1, bindings.size())).stream()
            .map( json -> expectObject( json))
            .map( otherProperties -> appendObject( firstProperty, otherProperties));
          })
        .collect( toList());
      }

    return decoded;
    }

  /**
   * If the given binding if for a simple (unexploded) property of the form, returns the binding.
   * Otherwise, returns <CODE>Optional.empty()</CODE>.
   */
  private Optional<Map.Entry<Key,String>> forFormProperty( Map.Entry<Key,String> binding)
    {
    return Optional.of( binding).filter(  b -> b.getKey().isFormProperty());
    }

  /**
   * Returns true if the given bindings represent an exploded object value.
   */
  private boolean isExplodedObject( String style, List<Map.Entry<Key,String>> bindings)
    {
    Map<String,List<Map.Entry<Key,String>>> explodedTypes = byExplodedType( bindings);

    boolean object =
      Optional.of( explodedTypes)
      .filter( types -> types.size() == 1)
      .map( types -> types.containsKey( "object"))
      .orElseThrow(
        () ->
        new IllegalStateException(
          bindings.isEmpty()?
          "No exploded property bindings found" :

          String.format(
            "Inconsistent bindings for style=%s: found bindings for %s and %s",
            style,
            explodedTypes.get( "array").get(0).getKey(),
            explodedTypes.get( "object").get(0).getKey())));

    if( object)
      {
      boolean expectDeep = "deepObject".equals( style);
      bindings.stream()
        .filter( binding -> expectDeep != binding.getKey().isDeepObjectProperty())
        .findFirst()
        .map( Map.Entry::getKey)
        .ifPresent( unexpected -> {
          throw new IllegalStateException( String.format( "Expected style=%s for property=%s but found value for %s", style, unexpected.getFormProperty(), unexpected));
          });
      }
      
    return object;
    }

  /**
   * Returns the given bindings grouped by the type of exploded value represented.
   */
  private Map<String,List<Map.Entry<Key,String>>> byExplodedType( List<Map.Entry<Key,String>> bindings)
    {
    return
      bindings.stream()
      .collect(
        groupingBy(
          binding -> binding.getKey().isFormProperty()? "array" : "object"));
    }

  /**
   * Returns the possible JSON object representations of the given exploded property value.
   */
  private List<JsonNode> decodeExploded( String style, List<Map.Entry<Key,String>> bindings)
    {
    return
      bindings.isEmpty()?
      emptyList() :

      isExplodedObject( style, bindings)?
      decodeExplodedObject( bindings) :

      bindings.size() == 1?
      decodeValue( style, bindings.get(0).getValue()) :

      decodeExplodedArray( bindings);
    }

  /**
   * Returns the possible JSON object representations of the given exploded array value.
   */
  private List<JsonNode> decodeExplodedArray( List<Map.Entry<Key,String>> bindings)
    {
    List<JsonNode> decoded;
    if( bindings.isEmpty())
      {
      decoded = singletonList( createArrayNode());
      }
    else
      {
      String element = bindings.get(0).getValue();
      List<JsonNode> decodedRest = decodeExplodedArray( bindings.subList( 1, bindings.size()));

      decoded = 
        decodeValue( "simple", element).stream()
        .map( json -> newArray( json))
        .flatMap( firstElement -> {
          return
            decodedRest.stream()
            .map( json -> expectArray( json))
            .map( otherElements -> appendArray( firstElement, otherElements));
          })
        .collect( toList());
      }

    return decoded;
    }

  /**
   * Returns the possible JSON object representations of the given property value.
   */
  private List<JsonNode> decodeValue( String style, String content)
    {
    return getValueDecoder( EncodingDef.forSimpleValue( style, false)).decode( content);
    }

  /**
   * Returns the content object property encodings.
   */
  public Map<String,EncodingDef> getPropertyEncodings()
    {
    return getContentDef().getPropertyEncodings();
    }

  /**
   * Returns the content encoding for the given property.
   */
  public EncodingDef getPropertyEncoding( String property)
    {
    return
      Optional.ofNullable( getPropertyEncodings().get( property))
      .orElse( EncodingDef.forUrlEncodedForm());
    }

  /**
   * Returns the content definition for this decoder.
   */
  private ContentDef getContentDef()
    {
    return contentDef_;
    }

  /**
   * Returns the value decoder for the given form property.
   */
  private SimpleDecoder getValueDecoder( EncodingDef encoding)
    {
    return new SimpleDecoder( encoding);
    }

  private final ContentDef contentDef_;

  /**
   * Represents a form binding key.
   */
  private static class Key
    {
    /**
     * Return the {@link Key} represented by the given name, or {@link Optional#empty} if none. 
     */
    public static Key of( ContentDef contentDef, String name)
      {
      return deepKeyOf( name).orElseGet( () -> explodedKeyOf( contentDef, name).orElseGet( () -> new Key( name)));
      }
    
    /**
     * Return the "deepObject"-style {@link Key} represented by the given name, or {@link Optional#empty} if none. 
     */
    private static Optional<Key> deepKeyOf( String name)
      {
      return
        Optional.of( DEEP_PROPERTY.matcher( name))
        .filter( matcher -> matcher.matches())
        .map( matcher -> new Key( matcher.group(1), matcher.group(2), true));      
      }

    /**
     * If <CODE>name</CODE> is a property of the form returns <CODE>Optional.empty()</CODE>. Otherwise, if P is a property of the form and
     * the value of P is an object with a property named <CODE>name</CODE> returns an exploded-style {@link Key}. Otherwise,
     * returns <CODE>Optional.empty()</CODE>.
     */
    private static Optional<Key> explodedKeyOf( ContentDef contentDef, String name)
      {
      return
        // Does the form have a property of the given name?
        Optional.ofNullable( contentDef.getSchema())
        .filter( form -> !form.has( name))
        .flatMap( form -> {
          return
            // No, so for all properties P defined by the form schema...
            schemaProperties( form).stream()
            .filter( P -> {
              return
                // ...and all properties defined by the P schema...
                schemaProperties( expectObject( P.getValue()))
                .stream()
                .map( Map.Entry::getKey)

                // ...does any property Q have the given name?
                .anyMatch( Q -> Q.equals( name));
              })
            .findFirst()
            .map( P -> new Key( P.getKey(), name, false));
          });
      }

    /**
     * Returns a list of the property definitions specified by the given schema.
     */
    private static List<Map.Entry<String,JsonNode>> schemaProperties( ObjectNode schema)
      {
      return
        Optional.ofNullable( schema)
        .flatMap( object -> asObject( object.get( "properties")))
        .map( properties -> toStream( properties.fields()).collect( toList()))
        .orElse( emptyList());      
      }

    /**
     * Creates a new Key instance.
     */
    private Key( String formProperty, String valueProperty, boolean deep)
      {
      formProperty_ = formProperty;
      valueProperty_ = valueProperty;
      deep_ = deep;
      }

    /**
     * Creates a new Key instance.
     */
    private Key( String formProperty)
      {
      this( formProperty, null, false);
      }

    /**
     * Returns a form property name.
     */
    public String getFormProperty()
      {
      return formProperty_;
      }

    /**
     * Returns a property name of the object that is the value of the {@link getObjectProperty form property}.
     */
    public String getValueProperty()
      {
      return valueProperty_;
      }

    /**
     * Returns true if this key identifies a foem property.
     */
    public boolean isFormProperty()
      {
      return getValueProperty() == null;
      }

    /**
     * Returns true if this key identifies an exploded object property.
     */
    public boolean isExplodedObjectProperty()
      {
      return !(isFormProperty() || isDeepObjectProperty());
      }

    /**
     * Returns true if this key identifies a "deepObject" property.
     */
    public boolean isDeepObjectProperty()
      {
      return deep_;
      }

    @Override
    public int hashCode()
      {
      return
        getClass().hashCode()
        ^ Objects.hashCode( getFormProperty())
        ^ Objects.hashCode( getValueProperty())
        ^ Objects.hashCode( isDeepObjectProperty());
      }

    /**
     * Returns true if this key and the other key have the same {@link getFormProperty form property}.
     */
    public boolean equalsFormProperty( Key other)
      {
      return Objects.equals( getFormProperty(), other.getFormProperty());
      }

    @Override
    public boolean equals( Object object) 
      {
      Key other =
        object != null && object.getClass().equals( getClass())
        ? (Key) object
        : null;
      
      return
        other != null
        && equalsFormProperty( other)
        && Objects.equals( getValueProperty(), other.getValueProperty())
        && Objects.equals( isDeepObjectProperty(), other.isDeepObjectProperty());
      }

    @Override
    public String toString()
      {
      String name =
        isDeepObjectProperty()?
        "DeepObject" :

        isExplodedObjectProperty()?
        "ExplodedObject" :

        "Form";
      
      return
        ToString.builder( name)
        .add( getFormProperty())
        .addIf( Optional.ofNullable( getValueProperty()))
        .toString();
      }
    
    private final String formProperty_; 
    private final String valueProperty_;
    private final boolean deep_;
    private static final Pattern DEEP_PROPERTY = Pattern.compile( "([^\\[]*)\\[([^\\]]*)\\]");
    }
  }
