//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.generator.*;
import static org.cornutum.tcases.DefUtils.*;
import static org.cornutum.tcases.generator.io.GeneratorSetDoc.*;
import static org.cornutum.tcases.generator.io.TupleGeneratorDoc.*;
import static org.cornutum.tcases.util.ObjectUtils.toObject;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.Closeable;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * An {@link IGeneratorSetSource} that reads from an XML document.
 *
 */
public class GeneratorSetDocReader extends DefaultHandler implements IGeneratorSetSource, Closeable
  {
  /**
   * The base class for element handlers used by this parser.
   *
   */
  protected abstract class ElementHandler extends DefaultHandler
    {
    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is not defined.
     */
    public String requireAttribute( Attributes attributes, String attributeName, String attributeValue) throws SAXException
      {
      if( attributeValue == null)
        {
        throw new SAXParseException( "No \"" + attributeName + "\" attribute specified", getDocumentLocator()); 
        }

      return attributeValue;
      }

    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      String id = requireAttribute( attributes, attributeName);

      try
        {
        assertIdentifier( id);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute", getDocumentLocator(), e); 
        }

      return id;
      }
      
    /**
     * Returns the value of the given attribute as a Long. Returns null if the attribute value is undefined or empty.
     */
    public Long getLongAttribute( Attributes attributes, String attributeName) throws SAXParseException
      {
      String value = getAttribute( attributes, attributeName);
      Long longValue = null;
      if( value != null)
        {
        try
          {
          longValue = Long.valueOf( value);
          }
        catch( NumberFormatException e)
          {
          throw
            new SAXParseException
            ( "Invalid \"" + attributeName + "\" attribute: \"" + value + "\" is not a number",
              getDocumentLocator()); 
          }
        }

      return longValue;
      }
      
    /**
     * Returns the value of the given attribute. Returns null if the attribute value is undefined or empty.
     */
    public String getAttribute( Attributes attributes, String attributeName)
      {
      return StringUtils.trimToNull( attributes.getValue( attributeName));
      }
      
    /**
     * Reports an error if the any of the given attributes are not valid for this element.
     */
    public void validateAttributes( String elementName, Attributes attributes) throws SAXException
      {
      Set<String> validAttributes = getValidAttributes();
      for( int i = attributes.getLength() - 1; i >= 0; i--)
        {
        if( !validAttributes.contains( attributes.getQName(i)))
          {
          throw
            new SAXParseException
            ( "Attribute=" + attributes.getQName(i)
              + " is not allowed for " + elementName + " elements",
              getDocumentLocator());
          }
        }
      }
      
    /**
     * Returns the valid attributes for this element.
     */
    protected Set<String> getValidAttributes()
      {
      return addAttributes( new HashSet<String>());
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      // No attributes to add.
      return attributes;
      }
      
    /**
     * Adds the given attribute list.
     */
    protected Set<String> addAttributeList( Set<String> attributes, String... attributeList)
      {
      Collections.addAll( attributes, attributeList);
      return attributes;
      }

    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return false;
      }
      
    /**
     * Changes the parent ElementHandler.
     */
    public void setParent( ElementHandler parent)
      {
      parent_ = parent;
      }

    /**
     * Returns the parent ElementHandler.
     */
    public ElementHandler getParent()
      {
      return parent_;
      }

    private ElementHandler parent_;
    }
  
  /**
   * Handles Generators elements.
   *
   */
  protected class GeneratorsHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return TUPLEGENERATOR_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setGeneratorSet( new GeneratorSet());
      }

    /**
     * Changes the {@link GeneratorSet} represented by this element.
     */
    private void setGeneratorSet( GeneratorSet generatorSet)
      {
      generatorSet_ = generatorSet;
      }

    /**
     * Returns the {@link GeneratorSet} represented by this element.
     */
    public GeneratorSet getGeneratorSet()
      {
      return generatorSet_;
      }
    }
  
  /**
   * Handles TupleGenerator elements.
   *
   */
  protected class TupleGeneratorHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return COMBINE_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      TupleGenerator tupleGenerator = new TupleGenerator();
      setTupleGenerator( tupleGenerator);
      tupleGenerator.setRandomSeed( getLongAttribute( attributes, SEED_ATR));

      Long defaultTupleSize = getLongAttribute( attributes, TUPLES_ATR);
      if( defaultTupleSize != null)
        {
        tupleGenerator.setDefaultTupleSize( defaultTupleSize.intValue());
        }

      String functionName = getAttribute( attributes, FUNCTION_ATR);
      if( functionName == null)
        {
        functionName = GeneratorSet.ALL;
        }
      else if( !functionName.equals( GeneratorSet.ALL))
        {
        try
          {
          assertIdentifier( functionName);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + FUNCTION_ATR + "\" attribute", getDocumentLocator(), e); 
          }
        }

      try
        {
        GeneratorsHandler parent = (GeneratorsHandler) getParent();
        parent.getGeneratorSet().addGenerator( functionName, tupleGenerator);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + FUNCTION_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link TupleGenerator} represented by this element.
     */
    private void setTupleGenerator( TupleGenerator tupleGenerator)
      {
      tupleGenerator_ = tupleGenerator;
      }

    /**
     * Returns the {@link TupleGenerator} represented by this element.
     */
    public TupleGenerator getTupleGenerator()
      {
      return tupleGenerator_;
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), SEED_ATR, TUPLES_ATR, FUNCTION_ATR);
      }

    private TupleGenerator tupleGenerator_;
    }
  
  /**
   * Handles Combine elements.
   *
   */
  protected class CombineHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        INCLUDE_TAG.equals( memberQname)
        || EXCLUDE_TAG.equals( memberQname)
        || ONCE_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      TupleCombiner tupleCombiner = new TupleCombiner();
      setTupleCombiner( tupleCombiner);

      Long tupleSize = getLongAttribute( attributes, TUPLES_ATR);
      if( tupleSize == null)
        {
        TupleGeneratorHandler parent = (TupleGeneratorHandler) getParent();
        tupleSize = new Long( parent.getTupleGenerator().getDefaultTupleSize());
        }
      tupleCombiner.setTupleSize( tupleSize.intValue());
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      TupleGeneratorHandler parent = (TupleGeneratorHandler) getParent();
      parent.getTupleGenerator().addCombiner( getTupleCombiner());
      }

    /**
     * Changes the {@link TupleCombiner} represented by this element.
     */
    private void setTupleCombiner( TupleCombiner tupleCombiner)
      {
      tupleCombiner_ = tupleCombiner;
      }

    /**
     * Returns the {@link TupleCombiner} represented by this element.
     */
    public TupleCombiner getTupleCombiner()
      {
      return tupleCombiner_;
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), TUPLES_ATR);
      }

    private TupleCombiner tupleCombiner_;
    }
  
  /**
   * Handles Include elements.
   *
   */
  protected class IncludeHandler extends ElementHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      CombineHandler parent = (CombineHandler) getParent();
      try
        {
        parent.getTupleCombiner().addIncludedVar( requireAttribute( attributes, VAR_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + VAR_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), VAR_ATR);
      }
    }
  
  /**
   * Handles Exclude elements.
   *
   */
  protected class ExcludeHandler extends ElementHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      CombineHandler parent = (CombineHandler) getParent();
      try
        {
        parent.getTupleCombiner().addExcludedVar( requireAttribute( attributes, VAR_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + VAR_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), VAR_ATR);
      }
    }
  
  /**
   * Handles tuple elements.
   *
   */
  protected abstract class TupleHandler extends ElementHandler
    {
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      addTuple( getTuple());
      }
    
    protected abstract void addTuple( TupleRef tuple) throws SAXParseException;

    /**
     * Retuns the TupleRef for this handler.
     */
    public TupleRef getTuple()
      {
      return tuple_;
      }

    private TupleRef tuple_ =  new TupleRef();
    }
  
  /**
   * Handles Once elements.
   *
   */
  protected class OnceHandler extends TupleHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return VAR_TAG.equals( memberQname);
      }
    
    protected void addTuple( TupleRef tuple) throws SAXParseException
      {
      CombineHandler parent = (CombineHandler) getParent();
      try
        {
        parent.getTupleCombiner().addOnceTuple( tuple);
        }
      catch( Exception e)
        {
        throw new SAXParseException ( "Invalid once-only tuple", getDocumentLocator(), e);
        }
      }
    }
  
  /**
   * Handles Var elements.
   *
   */
  protected class VarHandler extends ElementHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      TupleHandler parent = (TupleHandler) getParent();
      try
        {
        parent.getTuple()
          .addVarBinding(
            new VarBinding(
              requireAttribute( attributes, NAME_ATR),
              toObject( requireAttribute( attributes, VALUE_ATR))));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid variable binding", getDocumentLocator(), e); 
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR, VALUE_ATR);
      }
    }
  
  /**
   * Creates a new GeneratorSetDocReader object.
   */
  public GeneratorSetDocReader()
    {
    this( System.in);
    }
  
  /**
   * Creates a new GeneratorSetDocReader object.
   */
  public GeneratorSetDocReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link GeneratorSet} instance.
   */
  public IGeneratorSet getGeneratorSet()
    {
    SAXParser parser;
    try
      {
      parser = SAXParserFactory.newInstance().newSAXParser();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create SAXParser", e);
      }

    try
      {
      parser.parse( getInputStream(), this);
      }
    catch( SAXParseException spe)
      {
      throw
        new RuntimeException
        ( "Error in document at line=" + spe.getLineNumber(),
          spe);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read GeneratorSet", e);
      }

    return generatorSet_;
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ = stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return
      stream_==null
      ? System.in
      : stream_;
    }

  public void close()
    {
    IOUtils.closeQuietly( stream_);
    }

  /**
   * Pushes the given ElementHandler onto the top of the parse stack.
   */
  protected void pushElementHandler( ElementHandler handler)
    {
    elementHandlers_.add( 0, handler);
    }

  /**
   * Removes an ElementHandler from the top of the parse stack.
   */
  protected ElementHandler popElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.remove( 0) : null;
    }

  /**
   * Returns the ElementHandler at the top of the parse stack.
   */
  protected ElementHandler getCurrentElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.get( 0) : null;
    }
    
  public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
    {
    ElementHandler handler =
      qName.equals( COMBINE_TAG)?         (ElementHandler) new CombineHandler() :
      qName.equals( EXCLUDE_TAG)?         (ElementHandler) new ExcludeHandler() :
      qName.equals( GENERATORS_TAG)?      (ElementHandler) new GeneratorsHandler() :
      qName.equals( INCLUDE_TAG)?         (ElementHandler) new IncludeHandler() :
      qName.equals( ONCE_TAG)?            (ElementHandler) new OnceHandler() :
      qName.equals( TUPLEGENERATOR_TAG)?  (ElementHandler) new TupleGeneratorHandler() :
      qName.equals( VAR_TAG)?             (ElementHandler) new VarHandler() :
      null;

    if( handler == null)
      {
      throw new SAXParseException( "Unknown element: " + qName, getDocumentLocator()); 
      }

    ElementHandler parentHandler = getCurrentElementHandler();
    if( !(parentHandler == null
          ? GENERATORS_TAG.equals( qName)
          : parentHandler.isMember( qName)))
      {
      throw new SAXParseException( "The " + qName + " element is not allowed at this location", getDocumentLocator()); 
      }

    handler.validateAttributes( qName, attributes);
    handler.setParent( parentHandler);
    pushElementHandler( handler);
    handler.startElement( uri, localName, qName, attributes);
    }

  public void endElement( String uri, String localName, String qName) throws SAXException
    {
    ElementHandler handler = getCurrentElementHandler();
    if( handler != null)
      {
      handler.endElement( uri, localName, qName);
      popElementHandler(); 
      }
    }

  public void setDocumentLocator( Locator locator)
    {
    locator_ = locator;
    }

  public Locator getDocumentLocator()
    {
    return locator_;
    }
    
  public void warning( SAXParseException e) throws SAXException
    {
    }

  public void error( SAXParseException e) throws SAXException
    {
    throw e;
    }

  private InputStream           stream_;
  private Locator               locator_;
  private List<ElementHandler>  elementHandlers_  = new ArrayList<ElementHandler>();
  private GeneratorSet          generatorSet_;
  }
