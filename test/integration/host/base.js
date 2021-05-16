describe("host element handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    })
    afterEach(() => {
        container.remove();
    });

    it("basic host element creation", () => {
        const element = document.createElement("test-components-host-base");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
    });

    it("sibling element creation", () => {
        const element = document.createElement("test-components-host-siblings");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    });

    it("nested element creation", () => {
        const element = document.createElement("test-components-host-nested");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(2);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[0].childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].childNodes[0].tagName).toBe("IMG");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
    });

    it("attribute test", () => {
        const element = document.createElement("test-components-host-attributes");
        element.bar = "baz";
        element.foo = "fooo"

        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[0].className).toBe("foo");
        expect(element.shadowRoot.childNodes[0].part.value).toBe("baz");
        expect(element.shadowRoot.childNodes[0].title).toBe("combined fooo text");

        element.bar = "barbar"

        expect(element.shadowRoot.childNodes[0].part.value).toBe("barbar");
        expect(element.shadowRoot.childNodes[0].title).toBe("combined fooo text");

        element.foo = "foofoo"
        expect(element.shadowRoot.childNodes[0].title).toBe("combined foofoo text");
    })
});