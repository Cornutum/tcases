package org.cornutum;

import org.cornutum.examples.MyBaseClass

import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTest extends MyBaseClass {

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .queryParam( "Post Marks", "<Y> {X} #Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .queryParam( "Post Marks", "-98")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .queryParam( "Post Marks", "<Y> <Y> <Y> <Y>")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .queryParam( "Post Marks", "1U53/_Q41CZ68YH)c3/q\"S9&SeyuB/_a&J\"NSg`yJ&38{0]~J2NX74igr%E5EGc\\OigZb;Oe{5oWhA5|hu8QQ#eO@QBr\\B)45dU%wpU*Lq/%;JxdQHuGKPFqA>MQX^9[lVB8=e>fq\\XzA7T\"ILUr9sj.^y5t%1l}>dsaGQec@CT,J~pO!")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .pathParam( "attributes", ";approved=false;likes=507386362")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .pathParam( "attributes", ";attributes")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .pathParam( "attributes", ";attributes=577")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .pathParam( "attributes", ";likes=1034226798")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=;likes=539289752")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .pathParam( "attributes", ";approved=-539;likes=184728028")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;subject=;likes=50486665")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .pathParam( "attributes", ";approved=false;subject=H%g^2^2dfq}-seRK\"aJ{}.u0g<1ZR3_I0_[32?OV<=# W?N8@48te2#Jac4mHVJ+,48Bj_~j?AqZqSo,gyU;$'qV#M6RvTgzu/^iI-3Nr#ka{4E?}Dwv)?F*#D?f'R}Ho/#'JNN}*GZU&./+zl#/Y$|+&JRA~>=x.GJ;likes=707370706")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=false;likes=347939369;hkosltmgqzmjkfk=;hyhudkjuolivgmrm=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
