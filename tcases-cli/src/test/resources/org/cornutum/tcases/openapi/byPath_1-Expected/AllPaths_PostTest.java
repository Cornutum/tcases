package org.cornutum.examples;


import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.junit.Test;

import java.util.Map;
import static java.util.stream.Collectors.toMap;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class AllPaths_PostTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,2,1")
                .queryParam( "user attributes", "user type,Typical User")
            .when()
                .request( "HEAD", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
                .queryParam( "user attributes", (String) null)
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
                .queryParam( "user attributes", "-163.3")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
                .queryParam( "user attributes", "")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
                .queryParam( "user attributes", "user type,")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,1")
                .queryParam( "user attributes", "user type,*76s/1*r")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.user-type.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,2")
                .queryParam( "user attributes", "user type,VIP!,wfgawhzebqxw,-993.3,bt,-646,d,429")
            .when()
                .request( "HEAD", "/post")
            .then()
                // user-attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", (String) null)
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "-689.9")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,true")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,1,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Size=3
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,,1")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,,,NU,2")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,518413185,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,0")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.post-references.Items.Unique=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "post?", "post references,0,1,pfuxkykifiozux,g\"YtKR,&")
                .queryParam( "user attributes", "user type,VIP!")
            .when()
                .request( "HEAD", "/post")
            .then()
                // post.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "HEAD", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "HEAD", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> {X} #Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "#Z")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "{X} {X} {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", (String) null)
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "+")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Type=Not array
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "<Y> <Y> <Y> {X}")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Size=4
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "Post Marks", "#/lL\"%`\\M,7/RPK2]eZ/b\"LtW=MB6,(")
            .when()
                .request( "PATCH", "/post")
            .then()
                // Post-Marks.Items.Contains.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PATCH", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PATCH", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "884128300094585099.3")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "(?)")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "0")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "true")
                .formParam( "reviewer", "Me+You")
            .when()
                .request( "PUT", "/post")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", (String) null)
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "%")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Type=Not number
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "-1")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // postId.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "579110988210992054.5")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "100055597218570470.8")
                .contentType( "application/xml")
                .request().body( ">jg-FQI")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.Media-Type=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "545768800747318227.7")
                .contentType( "application/x-www-form-urlencoded")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "787781271506673512.7")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "string", "")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "887390451195556957.7")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "218911377319422868.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", (String) null)
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "847512139010470218.2")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "eytx", "7ea{")
                .formParam( "reviewer", "Larry Moe")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "711003745143914146.9")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "167771822150204639.4")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", (String) null)
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "886308504886987482.6")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "i&")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .queryParam( "postId", "920816794015899048.8")
                .contentType( "application/x-www-form-urlencoded")
                .formParam( "approved", "false")
                .formParam( "reviewer", "Larry Moe")
                .formParam( "dxobuuyffc", "true")
                .formParam( "wssfr", "true")
            .when()
                .request( "PUT", "/post")
            .then()
                // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "PUT", "/post", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "PUT", "/post", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,String> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( toMap( Header::getName, Header::getValue));
    }
}
