xdescribe("each loop handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    });
    afterEach(() => {
        container.remove();
    });

    it("each handling with filled array", () => {
        const element = document.createElement("test-components-helper-each-base-index");
        element.foo = ["foo", "bar", "baz"];
        element.bar = "mep";

        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(5);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[1].textContent).toBe("0-foo-mep")
        expect(element.shadowRoot.childNodes[2].textContent).toBe("1-bar-mep")
        expect(element.shadowRoot.childNodes[3].textContent).toBe("2-baz-mep")
        expect(element.shadowRoot.childNodes[4].tagName).toBe("FOOTER");
    });
});