//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;
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

    xmlWriter_.writeTagStart( SYSTEM_TAG);
    xmlWriter_.writeAttribute( NAME_ATR, systemInput.getName());
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( systemInput);
    toStream( systemInput.getFunctionInputDefs()).forEach( function -> writeFunction( function));
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( SYSTEM_TAG);
    }

  /**
   * Writes the given function input definition.
   */
  protected void writeFunction( FunctionInputDef function)
    {
    xmlWriter_.writeTagStart( FUNCTION_TAG);
    xmlWriter_.writeAttribute( NAME_ATR, function.getName());
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( function);
    for( String varType : function.getVarTypes())
      {
      writeInputs(
        varType,
        toStream( function.getVarDefs())
        .filter( varDef -> varDef.getType().equals( varType))
        .sorted());
      }
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( FUNCTION_TAG);
    }

  /**
   * Writes the given input variable list.
   */
  protected void writeInputs( String varType, Stream<IVarDef> varDefs)
    {
    xmlWriter_.writeTagStart( INPUT_TAG);
    xmlWriter_.writeAttribute( TYPE_ATR, varType);
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    varDefs.forEach( varDef -> writeVarDef( varDef));
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( INPUT_TAG);
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

    xmlWriter_.writeTagStart( varTag);
    xmlWriter_.writeAttribute( NAME_ATR, varDef.getName());
    conditionWriter.writeWhenAttribute();
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
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
    xmlWriter_.unindent();

    xmlWriter_.writeElementEnd( varTag);
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeValue( VarValueDef value)
    {
    ConditionWriter conditionWriter = new ConditionWriter( value.getCondition());

    xmlWriter_.writeTagStart( VALUE_TAG);
    xmlWriter_.writeAttribute( NAME_ATR, String.valueOf( value.getName()));
    conditionWriter.writeWhenAttribute();
    if( value.getType() == FAILURE)
      {
      xmlWriter_.writeAttribute( FAILURE_ATR, "true");  
      }
    else if( value.getType() == ONCE)
      {
      xmlWriter_.writeAttribute( ONCE_ATR, "true");  
      }
    writeProperties( propertyList( toStream( value.getProperties().getProperties())));
    xmlWriter_.writeTagEnd();

    xmlWriter_.indent();
    writeAnnotations( value);
    conditionWriter.writeWhenElement();
    xmlWriter_.unindent();
    
    xmlWriter_.writeElementEnd( VALUE_TAG);
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( IAnnotated annotated)
    {
    toStream( annotated.getAnnotations())
      .sorted()
      .forEach( annotation -> {
        xmlWriter_.writeTagStart( HAS_TAG);
        xmlWriter_.writeAttribute( NAME_ATR, annotation);
        xmlWriter_.writeAttribute( VALUE_ATR, annotated.getAnnotation( annotation));
        xmlWriter_.writeEmptyElementEnd();
        }); 
    }

  private void writeProperties( Optional<String> properties)
    {
    properties.ifPresent( p -> xmlWriter_.writeAttribute( PROPERTY_ATR, p));
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
        xmlWriter_.writeTagStart( WHEN_TAG);
        xmlWriter_.writeTagEnd();

        xmlWriter_.indent();
        condition_.accept( this);
        xmlWriter_.unindent();

        xmlWriter_.writeElementEnd( WHEN_TAG);
        }
      }

    public void writeWhenAttribute()
      {
      if( containsAll_ != null)
        {
        propertyList( toStream( containsAll_.getProperties()))
          .ifPresent( p -> xmlWriter_.writeAttribute( WHEN_ATR, p));
        }
      }
    
    public void visit( AllOf condition)
      {
      xmlWriter_.writeTagStart( ALLOF_TAG);
      writeProperties( propertiesOf( condition, ContainsAll.class));
      xmlWriter_.writeTagEnd();

      xmlWriter_.indent();
      visit( withoutPropertiesOf( condition, ContainsAll.class));
      xmlWriter_.unindent();

      xmlWriter_.writeElementEnd( ALLOF_TAG);
      }
  
    public void visit( AnyOf condition)
      {
      xmlWriter_.writeTagStart( ANYOF_TAG);
      writeProperties( propertiesOf( condition, ContainsAny.class));
      xmlWriter_.writeTagEnd();

      xmlWriter_.indent();
      visit( withoutPropertiesOf( condition, ContainsAny.class));
      xmlWriter_.unindent();

      xmlWriter_.writeElementEnd( ANYOF_TAG);
      }
  
    public void visit( ContainsAll condition)
      {
      xmlWriter_.writeTagStart( ALLOF_TAG);
      writeProperties( propertyList( toStream( condition.getProperties())));
      xmlWriter_.writeEmptyElementEnd();
      }
  
    public void visit( ContainsAny condition)
      {
      xmlWriter_.writeTagStart( ANYOF_TAG);
      writeProperties( propertyList( toStream( condition.getProperties())));
      xmlWriter_.writeEmptyElementEnd();
      }
  
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    public void visit( Not condition)
      {
      xmlWriter_.writeTagStart( NOT_TAG);
      writeProperties( propertiesOf( condition, ContainsAny.class));
      xmlWriter_.writeTagEnd();

      xmlWriter_.indent();
      visit( withoutPropertiesOf( condition, ContainsAny.class));
      xmlWriter_.unindent();

      xmlWriter_.writeElementEnd( NOT_TAG);
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
