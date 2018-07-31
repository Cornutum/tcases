//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
import org.cornutum.tcases.util.MapBuilder;
import org.cornutum.tcases.util.XmlWriter;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.io.SystemInputDoc.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Writes a {@link SystemInputDef} in the form of an XML document.
 *
 */
public class SystemInputDocWriter extends AbstractSystemInputWriter
  {
  /**
   * Creates a new SystemInputDocWriter object that writes to standard output.
   */
  public SystemInputDocWriter()
    {
    super();
    }
  
  /**
   * Creates a new SystemInputDocWriter object that writes to the given stream.
   */
  public SystemInputDocWriter( OutputStream stream)
    {
    super( stream);
    }
  
  /**
   * Creates a new SystemInputDocWriter object that writes to the given stream.
   */
  public SystemInputDocWriter( Writer writer)
    {
    super( writer);
    }

  /**
   * Writes the given system test definition the form of an XML document.
   */
  public void write( SystemInputDef systemInput)
    {
    xmlWriter_.writeDeclaration();

    xmlWriter_.writeElement(
      SYSTEM_TAG,
      MapBuilder.of( NAME_ATR, systemInput.getName()).build(),
      () ->
        {
        writeAnnotations( systemInput);
        toStream( systemInput.getFunctionInputDefs()).forEach( function -> writeFunction( function));
        });
    }

  /**
   * Writes the given function input definition.
   */
  protected void writeFunction( FunctionInputDef function)
    {
    xmlWriter_.writeElement(
      FUNCTION_TAG,
      MapBuilder.of( NAME_ATR, function.getName()).build(),
      () ->
        {
        writeAnnotations( function);
        for( String varType : function.getVarTypes())
          {
          writeInputs(
            varType,
            toStream( function.getVarDefs())
            .filter( varDef -> varDef.getType().equals( varType))
            .sorted());
          }
        });
    }

  /**
   * Writes the given input variable list.
   */
  protected void writeInputs( String varType, Stream<IVarDef> varDefs)
    {
    xmlWriter_.writeElement(
      INPUT_TAG,
      MapBuilder.of( TYPE_ATR, varType).build(),
      () ->
        {
        varDefs.forEach( varDef -> writeVarDef( varDef));
        });
    }

  /**
   * Writes the given variable definition.
   */
  protected void writeVarDef( IVarDef varDef)
    {
    Stream<VarValueDef> values =
      varDef.getValues() == null
      ? null
      : toStream( varDef.getValues());

    Stream<IVarDef> members = 
      varDef.getMembers() == null
      ? null
      : toStream( varDef.getMembers());

    String varTag =
      members == null
      ? VAR_TAG
      : VARSET_TAG;

    ConditionWriter conditionWriter = new ConditionWriter( varDef.getCondition());

    xmlWriter_.writeElement(
      varTag,
      MapBuilder.of( NAME_ATR, varDef.getName()).putIf( WHEN_ATR, conditionWriter.getWhenAttribute()).build(),
      () ->
        {
        writeAnnotations( varDef);
        conditionWriter.writeWhenElement();
        if( varTag.equals( VAR_TAG))
          {
          values.forEach( value -> writeValue( value));
          }
        else
          {
          members.forEach( member -> writeVarDef( member));
          }
        });
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeValue( VarValueDef value)
    {
    ConditionWriter conditionWriter = new ConditionWriter( value.getCondition());

    xmlWriter_.writeElement(
      VALUE_TAG,

      MapBuilder
        .of( NAME_ATR, String.valueOf( value.getName()))
        .putIf( FAILURE_ATR, Optional.ofNullable( value.getType() == FAILURE? "true" : null))
        .putIf( ONCE_ATR, Optional.ofNullable( value.getType() == ONCE? "true" : null))
        .putIf( WHEN_ATR, conditionWriter.getWhenAttribute())
        .putIf( PROPERTY_ATR, propertyList( toStream( value.getProperties().getProperties())))
        .build(),

      () ->
        {
        writeAnnotations( value);
        conditionWriter.writeWhenElement();
        });
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( IAnnotated annotated)
    {
    toStream( annotated.getAnnotations())
      .sorted()
      .forEach( annotation -> {
        xmlWriter_.writeElement(
          HAS_TAG,
          MapBuilder.of( NAME_ATR, annotation).put( VALUE_ATR, annotated.getAnnotation( annotation)).build());
        }); 
    }

  private Optional<String> propertyList( Stream<String> properties)
    {
    return
      properties
      .distinct()
      .sorted()
      .reduce( (list, property) -> list + ", " + property);
    } 

  /**
   * Flushes the writer.
   */
  public void flush() throws IOException
    {
    getXmlWriter().flush();
    }

  /**
   * Closes the writer.
   */
  public void close() throws IOException
    {
    getXmlWriter().close();
    }

  /**
   * Changes the output stream for this writer.
   */
  protected void setWriter( Writer writer)
    {
    setXmlWriter( new XmlWriter( writer));
    }

  /**
   * Changes the XmlWriter for this writer.
   */
  private void setXmlWriter( XmlWriter xmlWriter)
    {
    xmlWriter_ = xmlWriter;
    }

  /**
   * Returns the XmlWriter for this writer.
   */
  private XmlWriter getXmlWriter()
    {
    return xmlWriter_;
    }

  private class ConditionWriter implements IConditionVisitor
    {
    public ConditionWriter( ICondition condition)
      {
      condition_ = condition;

      containsAll_ =
        condition != null && condition.getClass().equals( ContainsAll.class)
        ? (ContainsAll) condition
        : null;
      }

    public void writeWhenElement()
      {
      if( condition_ != null && containsAll_ == null)
        {
        xmlWriter_.writeElement( WHEN_TAG, () -> { condition_.accept( this); });
        }
      }

    public Optional<String> getWhenAttribute()
      {
      return
        containsAll_ != null
        ? propertyList( toStream( containsAll_.getProperties()))
        : Optional.empty();
      }
    
    public void visit( AllOf condition)
      {
      xmlWriter_.writeElement(
        ALLOF_TAG,
        MapBuilder.optionalOf( PROPERTY_ATR, propertiesOf( condition, ContainsAll.class)),
        Optional.of( () ->
          {
          visit( withoutPropertiesOf( condition, ContainsAll.class));
          }));
      }
  
    public void visit( AnyOf condition)
      {
      xmlWriter_.writeElement(
        ANYOF_TAG,
        MapBuilder.optionalOf( PROPERTY_ATR, propertiesOf( condition, ContainsAny.class)),
        Optional.of( () ->
          {
          visit( withoutPropertiesOf( condition, ContainsAny.class));
          }));
      }
  
    public void visit( ContainsAll condition)
      {
      xmlWriter_.writeElement(
        ALLOF_TAG,
        MapBuilder.optionalOf( PROPERTY_ATR, propertyList( toStream( condition.getProperties()))),
        Optional.empty());
      }
  
    public void visit( ContainsAny condition)
      {
      xmlWriter_.writeElement(
        ANYOF_TAG,
        MapBuilder.optionalOf( PROPERTY_ATR, propertyList( toStream( condition.getProperties()))),
        Optional.empty());
      }
  
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    public void visit( Not condition)
      {
      xmlWriter_.writeElement(
        NOT_TAG,
        MapBuilder.optionalOf( PROPERTY_ATR, propertiesOf( condition, ContainsAny.class)),
        Optional.of( () ->
          {
          visit( withoutPropertiesOf( condition, ContainsAny.class));
          }));
      }
  
    private void visit( Stream<ICondition> conditions)
      {
      conditions.forEach( condition -> condition.accept( this));
      }

    private <T extends PropertyExpr> Optional<String> propertiesOf( ConditionSet condition, Class<T> propExprClass)
      {
      return
        propertyList(
          toStream( condition.getConditions())
          .filter( c -> c.getClass().equals( propExprClass))
          .flatMap( e -> toStream( ((PropertyExpr) e).getProperties())));
      }

    private <T extends PropertyExpr> Stream<ICondition> withoutPropertiesOf( ConditionSet condition, Class<T> propExprClass)
      {
      return
        toStream( condition.getConditions())
        .filter( c -> !c.getClass().equals( propExprClass));
      }

    private ICondition condition_;
    private ContainsAll containsAll_;
    }
  
  private XmlWriter xmlWriter_;
  }
